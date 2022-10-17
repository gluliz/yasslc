# yasslc
Yet another Simple Script Language Compiler

### Entrega de VE
Analisadores:
- Sintático;
- Escopo;
- Tipos.

Para gerar a tabela de ação, foi o utilizado o script que se encontra no repositório:
https://github.com/alexpizarroj/lalr1-table-generator

### Entrega de VC
- Analisador léxico.

## General instructions
### Linux
- Run the binary 'yasslc' on terminal with the '.ssl' file as an argument. Ex:
  ```
  yasslc test.ss
  ```
- If everything is fine, you should see an output file called 'output'. It is just a '.txt' file.
### Windows
- If you use a Windows PC, you will have to compile the file by yourself:
  - Download and install golang (https://go.dev/doc/install)
  - Clone this repo into your machine (git clone https://github.com/gluliz/yasslc)
  - Open a Powershell (or similar) and run ` go run . test.ss `

Feel free to test your own script. The grammar can be found in the file named "grammar_generator.txt". 