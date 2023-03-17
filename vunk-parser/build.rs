// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::env;
use std::fs::read_dir;
use std::fs::DirEntry;
use std::fs::File;
use std::io::Write;
use std::path::Path;

// build script's entry point
fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join("tests.rs");
    let mut test_file = File::create(&destination).unwrap();

    write_header(&mut test_file);

    let example_files = read_dir("./../vunk-examples/").unwrap();

    for file in example_files {
        write_test(&mut test_file, &file.unwrap());
    }
}

fn write_test(test_file: &mut File, file: &DirEntry) {
    let filepath = file.path().canonicalize().unwrap();
    let test_name = format!(
        "lexer_test_{}",
        filepath
            .file_name()
            .unwrap()
            .to_string_lossy()
            .replace(".vunk", "")
    );

    write!(
        test_file,
        include_str!("./tests/parser-test-template"),
        name = test_name,
        path = filepath.display(),
    )
    .unwrap();
}

fn write_header(test_file: &mut File) {
    write!(
        test_file,
        r#"
use chumsky::Parser;
"#
    )
    .unwrap();
}

