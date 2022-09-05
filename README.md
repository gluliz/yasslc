# yasslc
Yet another Simple Script Language Compiler

### Running the lexical part (on Linux)
- Run the binary 'yasslc' on terminal with the '.ssl' file as an argument. Ex:
  ```
  yasslc test.ss
  ```
- If everything is fine, you should see an output file called 'output'. It is just a '.txt' file.
### Running the lexical part (on Windows)
- If you use a Windows PC, you will have to compile the file by yourself:
  - Download and install golang (https://go.dev/doc/install)
  - Clone this repo into your machine (git clone https://github.com/gluliz/yasslc)
  - Open a Powershell (or similar) and run ` go run . test.ss `