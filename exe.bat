set NAME=png-diff
rem ��� ������� ���� ���� � ��� ���������� �������
set RACO=C:\"Tools\Racket\raco.exe" 
rd /S /Q bin
rd /S /Q make                  
mkdir bin
mkdir make
%RACO% exe --3m -o make\%NAME%.exe  png-diff.rkt
%RACO% distribute .\bin .\make\%NAME%.exe
