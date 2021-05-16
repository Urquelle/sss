# sss
<p align="center">
    <img src="https://github.com/Urquelle/sss/blob/master/misc/sss.png" />
    <br /><br />
</p>

## aktueller stand

derzeit werden folgende konstrukte, anweisungen, direktiven und ausdrücke unsterstützt.

* datenstrukturen (structs)
* globale variablen
* prozeduren
* prozeduraufrufe
* bedingte ausführungen
* schleifen
* mathematische ausdrücke

```sss
#load "W:/sss/data/expr.sss";

Vec3 :: struktur {
    x, y, z : s32;
}

z : s32 = 120;

master :: proc() -> s32 {
    a: u32 = 5;
    b: u32 = 3;
    c: u32 = 1;
    d: u32 = 2;
    e: u32 = 4;
    f: s32 = 7;

    test(*f);

    ## if
    falls e > 1 && falsch {
        res 5;
    }

    ## else if
    oder a < 1 {
        res 15;
    }

    ## while
    solange f < 5 {
        f += 1;
    }

    g : s32 = 0;

    ## for
    iter 1..3 {
        g += it;
    }

    v : Vec3;

    v.x = 5;

    h : [3] s32;
    (h)[c] = 12;

    res z - h[c] + g + v.x;
}

test :: proc(x : *s32 ) {
    @x = 15;
}
```
