<img src="https://github.com/Urquelle/sss/workflows/win10/badge.svg" />

# sss
<p align="center">
    <img src="https://github.com/Urquelle/sss/blob/master/misc/sss.png" />
    <br /><br />
</p>

compiler für eine c-ähnliche sprache.

```sss
#lade "W:/sss/data/expr.sss";

Vec3 :: struktur {
    x, y, z : u32;
}

master :: proc() -> u32 {
    a : s32 = 5;
    b : u32;
    c : [3][3] u32;
    v : Vec3;

    wenn a < 10 {
        test(*v.y);
    } sonst {
        res 33;
    }

    b = index(1);
    c[index(2)][b] = 15;

    res c[2][b];
}

test :: proc(x : *u32) {
    @x = 25;
}

index :: proc(i : u32) -> u32 {
    wenn i == 1 {
        res 1;
    } sonst {
        res 2;
    }
}
```

## aktueller stand

die gesamte ausführung geschieht aktuell auf basis des selbsterzeugten, x64-ähnlichen, bytecode und einer vm.

derzeit werden folgende konstrukte, anweisungen, direktiven und ausdrücke unsterstützt.

* datenstrukturen (structs)
* globale variablen
* prozeduren
* prozeduraufrufe
* bedingte ausführungen
* schleifen
* mathematische ausdrücke

das [beispiel1.sss](https://github.com/Urquelle/sss/tree/dev/beispiel/beispiel1.sss) wird zum bytecode transformiert und ausgeführt, das in [beispiel1.asm](https://github.com/Urquelle/sss/tree/dev/beispiel/beispiel1.asm)
