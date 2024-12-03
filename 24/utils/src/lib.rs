use std::fs;

pub fn get_input(file_path: &str) -> String {
    fs::read_to_string(file_path).unwrap()
}
