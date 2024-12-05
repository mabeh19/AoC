use std::fs;
use std::io::Write;


fn main() {
    fs::read_to_string("test.txt").map(solve1);
    fs::read_to_string("input.txt").map(solve1);
    fs::read_to_string("test2.txt").map(solve2);
    fs::read_to_string("input.txt").map(solve2);
}

const REG1: &str = r"^\((?<first>\d*),(?<second>\d*)\)";
const REG2: &str = r"(?:(don't\(\))*(do\(\))*(mul\((\d*),(\d*)\))*)*";
const REG3: &str = r"^mul\((?<first>\d*),(?<second>\d*)\).*";

fn solve1(input: String) {
    let re = regex::Regex::new(REG1).unwrap();
    let total = input.split("mul").fold(0, |acc, s| {
        re.captures(s)
            .map(|caps| 
                acc + caps["first"].parse::<i32>().unwrap_or(0) * caps["second"].parse::<i32>().unwrap_or(0)
            )
            .unwrap_or(acc)
    });

    println!("Total: {}", total);
}

fn solve2(mut input: String) {
    let mut mul_enabled = true;
    let mut total = 0;
    let re = regex::Regex::new(REG1).unwrap();

    while !input.is_empty() {
        if input.starts_with("don't()") {
            input = input.split_off(7);
            mul_enabled = false;
        }
        else if input.starts_with("do()") {
            input = input.split_off(4);
            mul_enabled = true;
        }
        else if input.starts_with("mul") {
            input = input.split_off(3);

            let mut len = 0;
            re.captures(&input)
                .map(|caps| {
                    len = caps[0].len();
                    if mul_enabled { total += caps["first"].parse::<i32>().unwrap_or(0) * caps["second"].parse::<i32>().unwrap_or(0) }
                });

            input = input.split_off(len);
        }
        else {
            input = input.split_off(1);
        }
    }
    //let re = regex::Regex::new(REG2).unwrap();
    //for i in re.captures_iter(&input) {
    //    match &i[0] {
    //        ""        => continue,
    //        "don't()" => mul_enabled = false,
    //        "do()"    => mul_enabled = true,
    //        _         => if mul_enabled { total += i[4].parse::<i32>().unwrap_or(0) * i[5].parse::<i32>().unwrap_or(0) } 
    //    }
    //}

    println!("Total: {}", total);
}


#[test]
fn reg_test() {
    let re = regex::Regex::new(REG1).unwrap();
    let s = "(2,4)%&";
    let caps = re.captures(s);

    println!("{:?}", caps);

    assert!(caps.is_some());
    let caps = caps.unwrap();
    assert_eq!(caps["first"], *"2");
    assert_eq!(caps["second"], *"4");
}

