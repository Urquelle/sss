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
    x, y, z : n32;
}

master :: proc() -> n32 {
    a : g32 = 5;
    b : n32;
    c : [3][3] n32;
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

test :: proc(x : *n32) {
    @x = 25;
}

index :: proc(i : n32) -> n32 {
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

## datentypen

### natürliche zahlen

natürliche zahlen sind positive, ganze zahlen angefangen bei 0. diese menge wird durch die spracheigenen datentypen n8, n16, n32 und n64 repräsentiert.

### ganze zahlen

ganze zahlen sind positive und negative ganze zahlen. diese menge wird durch die spracheigenen datentypen g8, g16, g32 und g64 repräsentiert.

### rationale zahlen

rationale zahlen sind alle zahlen, die einen quotienten zweier ganzer zahlen abbilden. diese menge wird durch die spracheigenen datentypen r32 und r64 repräsentiert.
