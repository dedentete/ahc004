g++ -o main.out main.cpp
cd tools
../main.out < in/0000.txt > out/0000.txt
cargo run --release --bin vis in/0000.txt out/0000.txt
cd ..