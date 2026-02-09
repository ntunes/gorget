# This file intentionally contains borrow checking errors.

void consume(String !s):
    pass

void main():
    # Error: use after move
    String s1 = "hello"
    String s2 = !s1
    print(s1)

    # Error: double move
    String s3 = "world"
    String s4 = !s3
    String s5 = !s3

    # Error: move inside loop
    String s6 = "loop"
    for i in 0..3:
        consume(!s6)
