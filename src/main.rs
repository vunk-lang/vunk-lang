#[tokio::main]
async fn main() -> Result<(), miette::Error> {
    function();
    println!("Hello World");
    Ok(())
}
