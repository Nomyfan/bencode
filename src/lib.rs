pub mod new;
use std::fmt::Display;

pub type BList = Vec<BNode>;
pub type BMap = std::collections::BTreeMap<String, BNode>;

pub enum BNode {
    Int(i64),
    Str(Vec<u8>),
    List(BList),
    Map(BMap),
}

impl BNode {
    pub fn marshal(&self, buf: &mut Vec<u8>) {
        match self {
            BNode::Int(i) => {
                buf.push(b'i');
                push_all(i.to_string().as_bytes(), buf);
                buf.push(b'e');
            }
            BNode::Str(s) => {
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
            BNode::Map(m) => {
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

    pub fn as_int(&self) -> Option<i64> {
        if let BNode::Int(int) = self {
            return Some(*int);
        }
        None
    }

    pub fn as_raw_str(&self) -> Option<&[u8]> {
        if let BNode::Str(s) = self {
            return Some(s);
        }
        None
    }

    pub fn as_string(&self) -> Option<String> {
        if let BNode::Str(s) = self {
            return Some(raw_str_to_string(s));
        }
        None
    }

    pub fn as_list(&self) -> Option<&BList> {
        if let BNode::List(lst) = self {
            return Some(lst);
        }
        None
    }

    pub fn as_map(&self) -> Option<&BMap> {
        if let BNode::Map(m) = self {
            return Some(m);
        }
        None
    }
}

impl Display for BNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            BNode::Int(i) => write!(f, "i{}e", i),
            BNode::Str(s) => write!(f, "{}:{}", &s.len(), &raw_str_to_string(s)),
            BNode::List(l) => {
                write!(f, "l")?;
                for n in l {
                    Display::fmt(&n, f)?;
                }
                write!(f, "e")
            }
            BNode::Map(m) => {
                write!(f, "d")?;
                for n in m {
                    let key = &n.0;
                    let val = &n.1;
                    write!(f, "{}:{}", key.len(), key)?;
                    Display::fmt(val, f)?;
                }
                write!(f, "e")
            }
        }
    }
}

fn raw_str_to_string(slice: &[u8]) -> String {
    let mut s = String::new();
    for x in slice {
        s.push(*x as char);
    }
    s
}

#[inline]
fn push_all(bytes: &[u8], buf: &mut Vec<u8>) {
    for x in bytes {
        buf.push(*x);
    }
}

pub fn parse<T>(stream: &mut T) -> Result<BNode, String>
where
    T: Iterator<Item = u8>,
{
    if let Some(delimiter) = stream.next() {
        let (node, _) = internal_parse(stream, delimiter, 0)?;
        if let Some(_) = stream.next() {
            return Err("Invalid stream".to_string());
        }
        return Ok(node);
    }
    Err("Invalid stream".to_string())
}

fn internal_parse<T>(
    stream: &mut T,
    delimiter: u8,
    position: usize,
) -> Result<(BNode, usize), String>
where
    T: Iterator<Item = u8>,
{
    match delimiter {
        b'l' => parse_list(stream, position),
        b'd' => parse_map(stream, position),
        b'0'..=b'9' => parse_string(stream, (delimiter - b'0') as usize, position),
        b'i' => parse_int(stream, position),
        _ => Err(format!(
            "Undefined delimiter: {}, position: #{}",
            delimiter, position
        )),
    }
}

fn parse_int<T>(stream: &mut T, position: usize) -> Result<(BNode, usize), String>
where
    T: Iterator<Item = u8>,
{
    let mut val = 0i64;
    let mut mul = 1;
    let mut next = 1usize;
    let mut cur_position = position;
    loop {
        match stream.next() {
            Some(c) => {
                cur_position += 1;
                match c {
                    b'-' if next == 1 => {
                        mul = -1;
                    }
                    b'e' if next != 1 => {
                        return Ok((BNode::Int(val * mul), cur_position));
                    }
                    b'0'..=b'9' => val = val * 10 + (c - b'0') as i64,
                    _ => {
                        return Err(format!(
                            "A number contains non-digit, position: #{}",
                            cur_position
                        ))
                    }
                }
                next = next + 1;
            }
            None => break,
        }
    }

    Err(format!(
        "Missing ending 'e' for a number, position: #{}",
        cur_position
    ))
}

fn parse_string<T>(stream: &mut T, init: usize, position: usize) -> Result<(BNode, usize), String>
where
    T: Iterator<Item = u8>,
{
    let mut len: usize = init;
    let mut matched = false;
    let mut raw_str: Vec<u8> = vec![];
    let mut cur_position = position;
    loop {
        match stream.next() {
            Some(c) => {
                cur_position += 1;
                if matched {
                    if len > 0 {
                        raw_str.push(c);
                        len = len - 1;
                    }
                    if len == 0 {
                        return Ok((BNode::Str(raw_str), cur_position));
                    }
                    continue;
                }
                match c {
                    b':' => {
                        matched = true;
                        raw_str = Vec::with_capacity(len);
                    }
                    b'0'..=b'9' => len = len * 10 + (c - b'0') as usize,
                    _ => {
                        return Err(format!(
                            "String's length contains non-digit, position: #{}",
                            cur_position
                        ))
                    }
                }
            }
            None => break,
        }
    }

    Err("String's length is shorter than expected".to_string())
}

fn parse_list<T>(stream: &mut T, position: usize) -> Result<(BNode, usize), String>
where
    T: Iterator<Item = u8>,
{
    let mut nodes = vec![];
    let mut cur_position = position;
    loop {
        match stream.next() {
            Some(c) => {
                cur_position += 1;
                if b'e' == c {
                    return Ok((BNode::List(nodes), cur_position));
                }
                let (node, up_pos) = internal_parse(stream, c, cur_position)?;
                cur_position = up_pos;

                nodes.push(node);
            }
            None => break,
        }
    }

    Err(format!("Missing 'e' at the end of list"))
}

fn parse_map<T>(stream: &mut T, position: usize) -> Result<(BNode, usize), String>
where
    T: Iterator<Item = u8>,
{
    let mut map = BMap::new();
    let mut key_turn = true;
    let mut key = vec![];
    let mut cur_position = position;
    loop {
        match stream.next() {
            Some(c) => {
                cur_position += 1;
                if b'e' == c {
                    return Ok((BNode::Map(map), cur_position));
                }
                if key_turn {
                    let (key_node, up_pos) = internal_parse(stream, c, cur_position)?;
                    let raw_key = match key_node {
                        BNode::Str(s) => s,
                        _ => {
                            return Err(format!(
                                "Dictionary key's type should be String, position: #{}",
                                cur_position
                            ))
                        }
                    };
                    cur_position = up_pos;

                    key.push(raw_str_to_string(&raw_key));
                } else {
                    let (val_node, up_pos) = internal_parse(stream, c, cur_position)?;
                    cur_position = up_pos;

                    map.insert(key.pop().unwrap(), val_node);
                }
                key_turn = !key_turn;
            }
            None => break,
        }
    }

    if !key_turn {
        return Err("A dictionary key lacks a corresponding value".to_string());
    }

    Err(format!(
        "Missing 'e' at the end of map, position: {}",
        cur_position
    ))
}

#[cfg(test)]
mod tests {
    fn str_to_raw(s: &str) -> Vec<u8> {
        let mut v = vec![];
        for x in s.bytes() {
            v.push(x);
        }

        v
    }

    #[test]
    fn test_bint_display() {
        let bi32 = crate::BNode::Int(32);
        assert_eq!(&format!("{}", bi32), "i32e");

        let neg_bi32 = crate::BNode::Int(-32);
        assert_eq!(&format!("{}", neg_bi32), "i-32e");
    }

    #[test]
    fn test_string_display() {
        let bstr = crate::BNode::Str(str_to_raw("str"));
        assert_eq!(&format!("{}", bstr), "3:str");
    }

    #[test]
    fn test_primitive_int() {
        let cases = vec!["i2147483648e", "i-253e"];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(bint) => {
                    if let crate::BNode::Int(i) = bint {
                        assert_eq!(i, x[1..x.len() - 1].parse::<i64>().unwrap());
                    }
                }
                Err(e) => panic!(e),
            }
        }
    }

    #[test]
    fn test_primitive_int_failed() {
        let cases = vec!["i2522", "ie", "i", "i-12-3e", "i13ee"];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(_) => panic!("Should fail"),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_primitive_string() {
        use crate::BNode;
        let cases = vec!["4:halo"];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(node) => {
                    if let BNode::Str(v) = node {
                        let index = x.find(':').unwrap();
                        assert_eq!(&x[index + 1..], &crate::raw_str_to_string(&v));
                    }
                }
                Err(e) => panic!(e),
            }
        }
    }

    #[test]
    fn test_primitive_string_failed() {
        let cases = vec!["5:hello2", "5:halo", "521"];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(_) => panic!("Should fail"),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_list() {
        let cases = vec!["l4:spami42ee", "le"];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(node) => assert_eq!(x, &format!("{}", node)),
                Err(e) => panic!(e),
            }
        }
    }

    #[test]
    fn test_list_failed() {
        let cases = vec!["l4:halo"];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(_) => panic!("Should fail"),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_nested_list() {
        let cases = vec!["ll5:helloe4:spami42ee"];

        for x in cases {
            match crate::parse(&mut x.bytes()) {
                Ok(node) => {
                    println!("{}-{}", &x, node);
                    assert_eq!(&format!("{}", node), &x);
                }
                Err(e) => panic!(e),
            }
        }
    }

    #[test]
    fn test_map() {
        let cases = vec![
            "d8:announce41:http://bttracker.debian.org:6969/announce13:creation datei15739038104ee",
        ];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(node) => {
                    assert_eq!(x, &format!("{}", node));
                }
                Err(e) => panic!(e),
            }
        }
    }

    #[test]
    fn test_map_failed() {
        let cases = vec!["d4:haloi23e", "di23e4:haloe"];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(_) => panic!("Should fail"),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_nested_map() {
        let cases = vec![
            r#"d8:announce41:http://bttracker.debian.org:6969/announce7:comment35:"Debian CD from cdimage.debian.org"13:creation datei1573903810e9:httpseedsl145:https://cdimage.debian.org/cdimage/release/10.2.0//srv/cdbuilder.debian.org/dst/deb-cd/weekly-builds/amd64/iso-cd/debian-10.2.0-amd64-netinst.iso145:https://cdimage.debian.org/cdimage/archive/10.2.0//srv/cdbuilder.debian.org/dst/deb-cd/weekly-builds/amd64/iso-cd/debian-10.2.0-amd64-netinst.isoe4:infod6:lengthi351272960e4:name31:debian-10.2.0-amd64-netinst.iso12:piece lengthi262144eee"#,
        ];
        for x in &cases {
            match crate::parse(&mut x.bytes()) {
                Ok(node) => {
                    assert_eq!(x, &format!("{}", node));
                }
                Err(e) => panic!(e),
            }
        }
    }
}
