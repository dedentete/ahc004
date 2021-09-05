n=$(($1-1))
g++ -o main.out main.cpp
cd tools
for i in `seq 0 $n`
do
    t=$(printf "%04d" $i)
    ../main.out < in/$t.txt > out/$t.txt
done
for i in `seq 0 $n`
do
    t=$(printf "%04d" $i)
    if [ $i = 0 ]; then
        cargo run --release --bin vis in/$t.txt out/$t.txt > scores.txt
    else 
        cargo run --release --bin vis in/$t.txt out/$t.txt >> scores.txt
    fi
done
python calc.py