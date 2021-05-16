# sss
<p align="center">
    <img src="https://github.com/Urquelle/sss/blob/master/misc/sss.png" />
    <br /><br />
</p>

compiler für eine c-ähnliche sprache.

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
