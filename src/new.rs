pub type BList = Vec<BNode>;
pub type BDict = std::collections::BTreeMap<String, BNode>;

#[derive(Debug)]
pub struct Error {
    pub position: i64,
    pub msg: String,
}

impl Error {
    fn new<T>(msg: T, position: i64) -> Error
    where
        T: Into<String>,
    {
        Error {
            msg: msg.into(),
            position,
        }
    }
}

macro_rules! throw {
    ($msg:expr, $pos:expr) => {
        return Err(Error::new($msg, $pos));
    };
}

pub type Result<T> = std::result::Result<T, Error>;

pub enum BNode {
    Number(i64),
    Stream(Vec<u8>),
    List(BList),
    Dict(BDict),
}

impl BNode {
    pub fn marshal(&self, buf: &mut Vec<u8>) {
        match self {
            BNode::Number(i) => {
                buf.push(b'i');
                push_all(i.to_string().as_bytes(), buf);
                buf.push(b'e');
            }
            BNode::Stream(s) => {
                push_all(s.len().to_string().as_bytes(), buf);
                buf.push(b':');
                push_all(s, buf);
            }
            BNode::List(l) => {
                buf.push(b'l');
                for bn in l {
                    bn.marshal(buf);
                }
                buf.push(b'e');
            }
            BNode::Dict(m) => {
                buf.push(b'd');
                for (k, v) in m {
                    push_all(k.len().to_string().as_bytes(), buf);
                    buf.push(b':');
                    push_all(k.as_bytes(), buf);
                    v.marshal(buf);
                }
                buf.push(b'e');
            }
        }
    }
}

#[inline]
fn push_all(bytes: &[u8], buf: &mut Vec<u8>) {
    for x in bytes {
        buf.push(*x);
    }
}

/**
https://en.wikipedia.org/wiki/Bencode
*/
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Token {
    NumberStart,
    NumberEnd,
    ListStart,
    ListEnd,
    DictStart,
    DictEnd,
    Length(i64),
    Colon,
    EOF,
}

#[derive(Debug)]
pub struct Lexer<'a, T>
where
    T: Iterator<Item = u8>,
{
    stream: &'a mut T,
    position: i64,
    cached_byte: Option<u8>,
    cached_token: Option<Token>,

    token_stack: Vec<Token>,
    current_token: Option<Token>,
}

impl<'a, T> Lexer<'a, T>
where
    T: Iterator<Item = u8>,
{
    fn new(stream: &'a mut T) -> Lexer<'a, T> {
        Lexer {
            stream,
            position: -1,
            cached_byte: None,
            cached_token: None,

            token_stack: vec![],
            current_token: None,
        }
    }

    fn next_byte(&mut self) -> Option<u8> {
        let next = match self.cached_byte {
            Some(_) => self.cached_byte.take(),
            None => {
                self.position += 1;
                self.stream.next()
            }
        };

        next
    }

    fn read_i64_before(&mut self, init: i64, symbol: u8) -> Result<(i64, i64)> {
        let mut num = init;
        let mut sign = 1i64;
        let mut read = 0;

        let mut meet = self.next_byte();
        while let Some(x) = meet {
            read += 1;

            match x {
                b'0'..=b'9' => {
                    if x == b'0' && sign == -1 && read == 2 {
                        throw!("Negative zero is not permitted", self.position)
                    }

                    if num == 0 && ((sign == 1 && read != 1) || (sign == -1 && read != 2)) {
                        throw!("Leading zero is not permitted", self.position)
                    }

                    num = num * 10 + (x - b'0') as i64
                }
                b'-' => match sign {
                    -1 if read != 1 => {
                        throw!(
                            "`-` can only appear in the head of the number",
                            self.position
                        )
                    }
                    _ => sign = -1,
                },
                b if b == symbol => {
                    self.cached_byte = Some(symbol);
                    return Ok((sign * num, read - 1));
                }
                _ => throw!("invalid number", self.position),
            }

            meet = self.next_byte();
        }

        throw!("invalid number", self.position)
    }

    fn read_nbytes(&mut self, len: usize) -> Result<Vec<u8>> {
        let mut ret = Vec::with_capacity(len);

        for _ in 0..len {
            match self.next_byte() {
                Some(byte) => ret.push(byte),
                None => {
                    throw!(
                        format!(
                            "stream's length is expected to be {}, but it's {}.",
                            len,
                            ret.len()
                        ),
                        self.position
                    );
                }
            }
        }

        Ok(ret)
    }

    fn next_token(&mut self) -> Result<Token> {
        if let Some(token) = self.cached_token.take() {
            return Ok(token);
        }

        match self.next_byte() {
            Some(unknown) => match unknown {
                b'i' => {
                    self.current_token = Some(Token::NumberStart);
                    self.token_stack.push(Token::NumberStart);

                    Ok(Token::NumberStart)
                }
                b'l' => {
                    self.current_token = Some(Token::ListStart);
                    self.token_stack.push(Token::ListStart);

                    Ok(Token::ListStart)
                }
                b'd' => {
                    self.current_token = Some(Token::DictStart);
                    self.token_stack.push(Token::DictStart);

                    Ok(Token::DictStart)
                }
                b'e' => match &self.token_stack.pop() {
                    Some(Token::NumberStart) => {
                        self.current_token = None;

                        Ok(Token::NumberEnd)
                    }
                    Some(Token::ListStart) => {
                        self.current_token = None;

                        Ok(Token::ListEnd)
                    }
                    Some(Token::DictStart) => {
                        self.current_token = None;

                        Ok(Token::DictEnd)
                    }
                    _ => {
                        throw!(
                            "`e` should be the end of number, list and dictionary.",
                            self.position
                        )
                    }
                },
                b'0'..=b'9' => {
                    // Get the stream length until it meets the colon
                    // TODO handle overflow?
                    let (length, _) = self.read_i64_before((unknown - b'0') as i64, b':')?;
                    self.current_token = Some(Token::Length(length));

                    Ok(Token::Length(length))
                }
                b':' => match &self.current_token {
                    Some(Token::Length(_)) => {
                        self.current_token = Some(Token::Colon);

                        Ok(Token::Colon)
                    }
                    _ => throw!("`:` should be after the length of stream.", self.position),
                },
                _ => throw!(format!("unknown token: {}", unknown), self.position),
            },
            None => Ok(Token::EOF),
        }
    }

    fn look_ahead(&mut self) -> Result<Token> {
        if let Some(token) = &self.cached_token {
            return Ok(token.clone());
        }

        let next_token = self.next_token()?;
        self.cached_token = Some(next_token);

        Ok(next_token)
    }
}

pub fn parse<T>(stream: &mut T) -> Result<BNode>
where
    T: Iterator<Item = u8>,
{
    let (node, mut _lexer) = parse_internal(Lexer::new(stream))?;

    match _lexer.next_token()? {
        Token::EOF => Ok(node),
        _ => throw!("Expect EOF", _lexer.position),
    }
}

fn parse_internal<'a, T>(mut lexer: Lexer<'a, T>) -> Result<(BNode, Lexer<'a, T>)>
where
    T: Iterator<Item = u8>,
{
    match lexer.look_ahead()? {
        Token::NumberStart => {
            let (number, _lexer) = parse_number(lexer)?;

            Ok((BNode::Number(number), _lexer))
        }
        Token::Length(_) => {
            let (stream, _lexer) = parse_stream(lexer)?;

            Ok((BNode::Stream(stream), _lexer))
        }
        Token::ListStart => {
            let (list, _lexer) = parse_list(lexer)?;

            Ok((BNode::List(list), _lexer))
        }
        Token::DictStart => {
            let (dict, _lexer) = parse_dict(lexer)?;

            Ok((BNode::Dict(dict), _lexer))
        }
        _ => throw!("invalid input", lexer.position),
    }
}

fn parse_number<'a, T>(mut lexer: Lexer<'a, T>) -> Result<(i64, Lexer<'a, T>)>
where
    T: Iterator<Item = u8>,
{
    assert_eq!(Token::NumberStart, lexer.next_token()?);

    let (value, read) = lexer.read_i64_before(0, b'e')?;

    if read < 1 {
        throw!("Number cannot be empty", lexer.position)
    }

    assert_eq!(Token::NumberEnd, lexer.next_token()?);

    Ok((value, lexer))
}

fn parse_stream<'a, T>(mut lexer: Lexer<'a, T>) -> Result<(Vec<u8>, Lexer<'a, T>)>
where
    T: Iterator<Item = u8>,
{
    let next_token = lexer.next_token()?;
    match next_token {
        Token::Length(len) => {
            assert_eq!(Token::Colon, lexer.next_token()?);
            let stream = lexer.read_nbytes(len as usize)?;

            Ok((stream, lexer))
        }
        _ => throw!("invalid input", lexer.position),
    }
}

fn parse_list<'a, T>(mut lexer: Lexer<'a, T>) -> Result<(BList, Lexer<'a, T>)>
where
    T: Iterator<Item = u8>,
{
    assert_eq!(Token::ListStart, lexer.next_token()?);
    let mut list = vec![];

    loop {
        match lexer.look_ahead()? {
            Token::NumberStart => {
                let (number, _lexer) = parse_number(lexer)?;
                list.push(BNode::Number(number));

                lexer = _lexer;
            }
            Token::Length(_) => {
                let (stream, _lexer) = parse_stream(lexer)?;
                list.push(BNode::Stream(stream));

                lexer = _lexer;
            }
            Token::ListStart => {
                let (_list, _lexer) = parse_list(lexer)?;
                list.push(BNode::List(_list));

                lexer = _lexer;
            }
            Token::DictStart => {
                let (dict, _lexer) = parse_dict(lexer)?;
                list.push(BNode::Dict(dict));

                lexer = _lexer;
            }
            Token::ListEnd => {
                lexer.next_token()?;
                return Ok((list, lexer));
            }
            _ => {
                throw!("invalid list", lexer.position);
            }
        }
    }
}

fn parse_dict<'a, T>(mut lexer: Lexer<'a, T>) -> Result<(BDict, Lexer<'a, T>)>
where
    T: Iterator<Item = u8>,
{
    assert_eq!(Token::DictStart, lexer.next_token()?);
    let mut dict = BDict::new();
    loop {
        match lexer.look_ahead()? {
            Token::Length(_) => {
                let (raw_key, _lexer) = parse_stream(lexer)?;
                let key = String::from_utf8(raw_key).unwrap();
                let (value, _lexer) = parse_internal(_lexer)?;

                lexer = _lexer;

                dict.insert(key, value);
            }
            Token::DictEnd => {
                lexer.next_token()?;
                return Ok((dict, lexer));
            }
            _ => throw!("invalid dictionary", lexer.position),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{parse, parse_dict, parse_number, parse_stream, BNode, Lexer, Token};

    #[test]
    fn test_lexer_read_i64_before() {
        let raws = ["2147483648e", "0e"];
        let ret = [2147483648, 0];

        for i in 0..raws.len() {
            let raw = raws[i];
            let mut bytes = raw.bytes();
            let mut lexer = Lexer::new(&mut bytes);

            let (value, _) = lexer.read_i64_before(0, b'e').unwrap();
            assert_eq!(ret[i], value);
        }
    }

    #[test]
    fn test_lexer_read_negative_zero() {
        let raw = "-0e";

        let mut bytes = raw.bytes();
        let mut lexer = Lexer::new(&mut bytes);

        let _ = lexer
            .read_i64_before(0, b'e')
            .expect_err("Negative zero is not permitted");
    }

    #[test]
    fn test_lexer_no_leading_zero() {
        let raws = ["00e", "01e"];

        for raw in raws.iter() {
            let mut bytes = raw.bytes();
            let mut lexer = Lexer::new(&mut bytes);

            let _ = lexer
                .read_i64_before(0, b'e')
                .expect_err("Leading zero is not permitted");
        }
    }

    #[test]
    fn test_lexer_read_nbytes() {
        let raw = "bencode";

        let mut bytes = raw.bytes();
        let mut lexer = Lexer::new(&mut bytes);

        let raw_bytes = lexer.read_nbytes(3).unwrap();
        assert_eq!("ben".as_bytes(), &raw_bytes);

        let raw_bytes = lexer.read_nbytes(4).unwrap();
        assert_eq!("code".as_bytes(), &raw_bytes);
    }

    #[test]
    fn test_lexer_position_case1() {
        let raw = "bencode";

        let mut bytes = raw.bytes();
        let mut lexer = Lexer::new(&mut bytes);

        let _ = lexer.read_nbytes(3).unwrap();
        assert_eq!(2, lexer.position);

        let _ = lexer.read_nbytes(4).unwrap();
        assert_eq!(6, lexer.position);
    }

    #[test]
    fn test_lexer_position_case2() {
        let raw = "i56e";

        let mut bytes = raw.bytes();
        let mut lexer = Lexer::new(&mut bytes);

        let _ = lexer.look_ahead().unwrap();
        assert_eq!(0, lexer.position);

        let _ = lexer.look_ahead().unwrap();
        assert_eq!(0, lexer.position);
    }

    #[test]
    fn test_lexer_position_case3() {
        let raw = "7:bencode";

        let mut bytes = raw.bytes();
        let mut lexer = Lexer::new(&mut bytes);

        let _ = lexer.look_ahead().unwrap();
        assert_eq!(1, lexer.position);
    }

    #[test]
    fn test_lexer_position_case4() {
        let raw = "i-2-0e";

        let mut bytes = raw.bytes();
        let lexer = Lexer::new(&mut bytes);

        assert_eq!(3, parse_number(lexer).unwrap_err().position)
    }

    #[test]
    fn test_lexer_look_ahead() {
        let raw = "i256e";

        let mut bytes = raw.bytes();
        let mut lexer = Lexer::new(&mut bytes);

        assert_eq!(Token::NumberStart, lexer.look_ahead().unwrap());
        assert_eq!(Token::NumberStart, lexer.look_ahead().unwrap());
    }

    #[test]
    fn test_parse_number() {
        let raw = ["i256e", "i-1024e"];
        let expected = [256, -1024];
        let len = raw.len();

        for x in 0..len {
            let str = raw[x];

            let mut bytes = str.bytes();
            let lexer = Lexer::new(&mut bytes);

            let (value, _lexer) = parse_number(lexer).unwrap();
            assert_eq!(expected[x], value);
        }
    }

    #[test]
    fn test_parse_number_failed() {
        let cases = vec!["i2522", "ie", "i", "i-12-3e", "i13ee"];
        let len = cases.len();
        for i in 0..len {
            let x = cases[i];
            match parse(&mut x.bytes()) {
                Ok(_) => panic!("{}-th should fail", i),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_stream() {
        let raw = "7:bencode";

        let mut bytes = raw.bytes();
        let lexer = Lexer::new(&mut bytes);

        let (stream, _lexer) = parse_stream(lexer).unwrap();
        assert_eq!("bencode".as_bytes(), &stream);
    }

    #[test]
    fn test_parse_stream_failed() {
        let cases = vec!["5:hello2", "5:halo", "521"];
        let len = cases.len();
        for i in 0..len {
            let x = cases[i];
            match parse(&mut x.bytes()) {
                Ok(_) => panic!("{}-th should fail", i),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_list() {
        let cases = vec!["li256e7:bencodeli256e7:bencodeee", "l4:spami42ee", "le"];
        let len = cases.len();
        for i in 0..len {
            let x = cases[i];
            match parse(&mut x.bytes()) {
                Ok(node) => {
                    let mut buf = vec![];
                    node.marshal(&mut buf);
                    assert_eq!(x.as_bytes(), &buf)
                }
                Err(e) => std::panic::panic_any(e),
            }
        }
    }

    #[test]
    fn test_parse_list_failed() {
        let cases = vec!["l4:halo"];
        let len = cases.len();
        for i in 0..len {
            let x = cases[i];
            match parse(&mut x.bytes()) {
                Ok(_) => panic!("{}-th should fail", i),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_nested_list() {
        let raw = "ll5:helloe4:spami42ee";

        let mut bytes = raw.bytes();
        let bnode = parse(&mut bytes).unwrap();

        let mut buf = vec![];
        bnode.marshal(&mut buf);

        assert_eq!(raw.as_bytes(), &buf);
    }

    #[test]
    fn test_parse_dict() {
        let raw = "d3:bar4:spam3:fooi42ee";

        let mut bytes = raw.bytes();
        let lexer = Lexer::new(&mut bytes);

        let (dict, _lexer) = parse_dict(lexer).unwrap();
        assert_eq!(2, dict.len());

        match dict.get("bar").unwrap() {
            BNode::Stream(stream) => {
                assert_eq!(&stream, &"spam".as_bytes());
            }
            _ => panic!("`bar` should have the value `spam`"),
        }

        match dict.get("foo").unwrap() {
            BNode::Number(iv) => {
                assert_eq!(&42, iv);
            }
            _ => panic!("`foo` should have the value `42`"),
        }
    }

    #[test]
    fn test_parse_dict_failed() {
        let cases = vec!["d4:haloi23e", "di23e4:haloe"];
        for x in &cases {
            match parse(&mut x.bytes()) {
                Ok(_) => panic!("Should fail"),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_nested_dict() {
        let raw = r#"d8:announce41:http://bttracker.debian.org:6969/announce7:comment35:"Debian CD from cdimage.debian.org"13:creation datei1573903810e9:httpseedsl145:https://cdimage.debian.org/cdimage/release/10.2.0//srv/cdbuilder.debian.org/dst/deb-cd/weekly-builds/amd64/iso-cd/debian-10.2.0-amd64-netinst.iso145:https://cdimage.debian.org/cdimage/archive/10.2.0//srv/cdbuilder.debian.org/dst/deb-cd/weekly-builds/amd64/iso-cd/debian-10.2.0-amd64-netinst.isoe4:infod6:lengthi351272960e4:name31:debian-10.2.0-amd64-netinst.iso12:piece lengthi262144eee"#;

        let mut bytes = raw.bytes();
        let bnode = parse(&mut bytes).unwrap();

        let mut buf = Vec::with_capacity(bytes.len());
        bnode.marshal(&mut buf);

        assert_eq!(&raw.as_bytes(), &buf);
    }
}
