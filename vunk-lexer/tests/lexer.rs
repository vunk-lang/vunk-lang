const FILES: [&str; 32] = [
    "0001.vunk",
    "0002.vunk",
    "0003.vunk",
    "0004.vunk",
    "0005.vunk",
    "0006.vunk",
    "0007.vunk",
    "0008.vunk",
    "0009.vunk",
    "0010.vunk",
    "0011.vunk",
    "0012.vunk",
    "0013.vunk",
    "0014.vunk",
    "0015.vunk",
    "0016.vunk",
    "0017.vunk",
    "0018.vunk",
    "0019.vunk",
    "0020.vunk",
    "0021.vunk",
    "0022.vunk",
    "0023.vunk",
    "0024.vunk",
    "0025.vunk",
    "0026.vunk",
    "0027.vunk",
    "0028.vunk",
    "0029.vunk",
    "0030.vunk",
    "0031.vunk",
    "0032.vunk",
];

use chumsky::Parser;

#[test]
fn test_lex() {
    let examples_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("vunk-examples");
    for filename in FILES {
        let filepath = examples_dir.join(filename);
        let code =
            std::fs::read_to_string(&filepath).expect(&format!("Reading {}", filepath.display()));

        let lexer = vunk_lexer::lexer();
        let (tokens, errs) = lexer.parse_recovery(code);
        assert!(
            errs.is_empty(),
            "{}: Tokens: {:?}, Errors: {}",
            filepath.display(),
            tokens,
            errs.into_iter()
                .map(|e| format!("{}", e))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }
}
