#Instrukcja rozwijania gramatyki i tworzenia sparsowanych korpusów

##Założenia początkowe

Dysponujemy korpusem znajdującym się w pliku:
```
mytask/corpus.tab
```
w każdym wersie pliku znajduje się pojedynczy tekst do sparsowania.

Sparsowany korpus znajduje się w pliku:
```
mytask/corpus.json
```
Początkowo plik ten może być pustą listą.

W pliku 
```
mytask/myschema.json
```
znajdudej się schemat formatu JSON dla sparsowanego korpusu.

W katalogu 
```
mytask/data
```
znajduje się wstępna, być może pusta wersja gramatyki i leksykonów.

W katalogu 
```
mytask/mytheory
```
znajduje się wstępna, być może pusta wersja lokalnej dla `mytask` teorii `mytheory`.

Chcemy też korzystać z dostarczonej z ENIAM-em teorii `numbers`.

##Parsowanie pojedynczych tekstów

Korzystamy z parsera `eniam`

Uruchamianie w wersji z wydzielonym programem `subsyntax`:
```
cd mytask
subsyntax -m -p 1234 -a --def-cat -u mytheory -e numbers
eniam --port 1234 --no-disamb --line-mode -u mytheory  -e numbers --no-sem
eniam --port 1234 --no-disamb -v 2 -u mytheory -e numbers --partial -t
eniam --port 1234 --no-disamb --line-mode -u mytheory -e numbers --partial -j
```

Uruchamianie w wersji z wbudowanym programem `subsyntax`:
```
cd mytask
eniam -a --def-cat --no-disamb --line-mode -u mytheory  -e numbers --no-sem
eniam -a --def-cat --no-disamb -v 2 -u mytheory -e numbers --partial -t
eniam -a --def-cat --no-disamb --line-mode -u mytheory -e numbers --partial -j
```

Trzy następujące po sobie wywołania parsera ENIAM różnią się tym, że:
- w pierwszym generowana jest jedynie struktura zależnościowa
- w drugim generowany jest graf pojęć wraz z wstępną konwersją do formatu JSON
- w trzecim generowany jest ostateczny format JSON

Pierwsze dwa wywołania służą przede wszystkim do wykrywania i usuwania błędów w gramatyce.

Uruchamianie tak, by uzyskać zapis przetwarzania semantycznego
```
cd mytask
subsyntax -m -p 1234 -a --def-cat -u mytheory -e numbers
eniam --port 1234 --no-disamb -u mytheory -e numbers --partial -v 2 --no-infer
```
Służy do usuwania błędów na etapie konwersji z drzewa zależnościowego do grafu pojęć.

##Praca z korpusem

Program `vereniam` parsuje korpus i dzieli go na część sparsowaną i zwalidowaną pod względem schematu JSON, 
sparsowaną i niezwalidowaną oraz niesparsowaną.
```
cd mytask
vereniam -a --def-cat --no-disamb -u mytheory -e numbers --partial -k myschema -r record -p . -f corpus
```

Po opracowaniu wstępnej gramatyki używamy program `vereniam` do wydzielenia z korpusu tekstów, które parsują się przy tej gramatyce. 
Następnie trzeba te teksty przejrzeć pod względem poprawności i przenieść do sparsowanego korpusu, potem rozwinąć gramatykę i kontynuować iteracyjnie. Jeśli zachodzi potrzeba przetworzenia całego korpusu, można stworzyć aliasy upraszczające szczególnie oporne przykłady lub napisać dla nich reprezentację semantyczną ręcznie.

W trakcie iteracyjnego rozwijania gramatyki przykłady sparsowane za pomocą jej wcześniejszej wersji mogą przestać się poprawnie parsować. Program `validator` parsuje ponownie sparsowany korpus za pomocą aktualnej gramatyki i wskazuje, dla których przykładów wynik jest odmienny niż w korpusie.
```
cd mytask
validator -a --def-cat --no-disamb -u mytheory -e numbers --partial -p . -f corpus
```

Możemy też uruchomić `validator` z zewnętrznym ENIAMem:
```
cd mytask
eniam -p 9760 -a --def-cat --no-disamb -u mytheory -e numbers --partial -j
validator --port2 9760 --partial -p . -f corpus
```
Jest to przydatne, gdy chcemy na raz zwalidować wiele plików ze sparsowanymi korpusami.

Program `schema` sprawdza zgodność sparsowanego korpusu z schemat formatu JSON tego pliku.
```
cd mytask
schema -k myschema -r record -p . -f corpus
```
Jest on przydatny do weryfikacji ręcznie napisanych reprezentacji semanycznych.

Program `selector` wskazuje tekstu z korpusu, które nie znajdują się w jego sparsowanej wersji.
```
cd mytask
schema -k myschema -r record -p . -f corpus
selector -s . -t corpus -t corpus2 -p . -f newcorpus
```
Można go stosować, gdy przy pojawią się nowe teksty rozszerzające już istniejące korpusy.




