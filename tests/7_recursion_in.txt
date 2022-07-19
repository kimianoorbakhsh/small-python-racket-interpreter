def fact(n = 0):
    if n < 2:
        return 1;
    else:
        a = n;
        b = fact(n - 1);
        return a * b;
    ;
;

f = fact(7);
print(f);