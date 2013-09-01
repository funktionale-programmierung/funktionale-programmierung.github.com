---
layout: post
description: Haskell FFI
title: "Haskell FFI"
author: johannes-weiss
tags: ["Haskell", "FFI", "C"]
---

Obwohl für Haskell auf [Hackage](http://hackage.haskell.org/) eine sehr große
Menge von Bibliotheken bereit steht, kann es trotzdem vorkommen, dass keine
Bibliothek das gewünschte leistet. Sollte es eine Bibliothek für eine andere
Sprache geben, kann man diese nach Haskell portieren oder eine vergleichbare
Bibliothek für Haskell erschaffen. In manchen Fällen ist das die richtige
Entscheidung, in anderen Fällen kann das sehr viel Arbeit bedeuten. Auch
Performanz kann manchmal ein Grund sein bestimmte Funktionen in zum Beispiel in
C zu implementieren. Und genau das ist das Thema dieses Blogposts: Von Haskell
aus C-Funktionen aufrufen. Der Post richtet sich hauptsächlich an Programmierer
die schon etwas Erfahrung in Haskell haben, Spezialkenntnisse oder
Haskell-Expertenwissen sind jedoch nicht erforderlich.


<!-- more start -->

<!-- Das ist auch die Syntax für Kommentare, die im HTML nachher
auftauchen. -->

## Generelles ##

Um von Haskell C-Funktionen aufzurufen benutzt man in der Regel das [Haskell
Foreign Function
Interface](http://www.haskell.org/haskellwiki/Foreign_Function_Interface), kurz
FFI. Mit dem FFI lassen sich einige Funktionen schon recht einfach einbinden, zum
Beispiel die Sinus-Funktion.

    {-# INCLUDE <math.h> #-}
    {-# LANGUAGE ForeignFunctionInterface #-}
    module FfiExample where
    import Foreign.C -- get the C types

    -- pure function
    foreign import ccall "sin" c_sin :: CDouble -> CDouble

    sin :: Double -> Double
    sin d = realToFrac (c_sin (realToFrac d))

Das ist ausreichend um die C-Funktion `sin()` in Haskell benutzbar zu machen. Das
FFI alleine ist für einige Funktionen schon ausreichend einfach zu nutzen,
allerdings kann es für kompliziertere Funktionen hilfreich sein, wenn einem auch
noch Konversion zwischen C- und Haskell-Datentypen abgenommen oder zumindest
vereinfacht wird. Im vorherigen Beispiel ist nur Konversion vom C-Typen
`CDouble` (`double` in C) zu Haskells `Double` notwendig. Für nicht-primitive
Datentypen kann das aber mehr Arbeit bedeuten, deshalb setzt dieser Artikel auf
[c2hs](https://github.com/haskell/c2hs), was solche Konversionen (im weiteren
"Marshalling" genannt) und die generelle Benutzung von C Funktionen vereinfacht.
C2hs selbst hat zur Dokumentation ein [Wiki][c2hs-wiki] und einen [User
Guide][c2hs-guide] der bei weiteren Fragen helfen sollten.

In diesem Blogpost wird eine beispielhafte (aber funktionierende), fragwürdige
C-Verschlüsselungsbibliothek mit dem Namen `shonky-crypt` an Haskell angebunden.
Weder die C-, noch die Haskell-Version sollten für irgendwelche realen
Verschlüsselungen eingesetzt werden. Beide existieren nur dieses Blogposts wegen
und bieten keinerlei Sicherheit. Der Code inklusive einer Cabal-Datei mit der
man das Gesamt-Projekt in einem Schritt kompilieren kann ist im [GitHub-Projekt
`hs-shonky-crypt`][github] verfügbar. Eine für dieses Beispiel wichtige Datei
ist [`src-c/shonky-crypt/shonky-crypt.h`][h] (Dateinamen anklicken um die Datei
zu sehen) welche die C-API der Bibliothek definiert und dokumentiert. Die
Haskell-Anbindung der Funktionen soll nun schrittweise durchexerziert werden.
Die C-Bibliothek wurde mit dem Gedanken entworfen, beispielhaft zu illustrieren
was bei vielen C-Bibliotheken zur Anbindung notwendig wäre.

## API-Anbindung ##

### Anbindung von reinen Funktionen ###

Die Bibliothek enthält eine sehr einfache Funktion

    double sc_entropy(const char *str, size_t len);

mit der sich die [Shannon-Entropie][shannon] einer Zeichenkette berechnen lässt.
Für Haskell wäre beispielsweise eine Funktion mit folgendem Prototypen
wünschenswert:

    entropy :: ByteString -> Double

Die Implementierung sollte einfach die C-Funktion `sc_entropy()` aufrufen.  C2hs
verlangt dazu eine spezielle c2hs-Datei (Datei-Endung `.chs`), welche sowohl
normalen Haskell-Code als auch speziellen c2hs-Präprozessor-Code enthält. C2hs
wandelt den Präprozessor-Code dann in normalen Haskell-Code unter Benutzung des
FFI um. Die `.chs` Datei für die hier angebundene Beispiel-Bibliothek findet
sich unter [`src-hs/Codec/ShonkyCrypt/ShonkyCryptFFI.chs`][chs]. Der
Code-Schnipsel der notwendig ist um die Funktion `sc_entropy()` Funktion mit
c2hs in Haskell benutzen zu können ist im folgenden Code-Block zu sehen.

    #include "shonky-crypt.h"

    -- [...]

    type CSizeT = {# type size_t #}

    withByteStringLen :: ByteString -> ((CString, CSizeT) -> IO a) -> IO a
    withByteStringLen str f = BSU.unsafeUseAsCStringLen str (\(cstr, len) ->
        f (cstr, fromIntegral len))

    {#fun pure unsafe sc_entropy as
        ^ { withByteStringLen *`ByteString'& } -> `Double' #}

Aber der Reihe nach:

1. `#include "shonky-crypt.h"` tut nichts anderes als das Einbinden der
   C-Header-Datei, wie aus normalen C-Programmen bekannt. Ab sofort können die
   Definitionen der Header-Datei in c2hs-Direktiven verwendet werden
2. `type CSizeT = {# type size_t #}` definiert den Haskell-Typen `CSizeT` genau
   so wie `size_t` in C definiert ist (beispielsweise `unsigned int` (also
   `CUInt` in Haskell) für 32-bit Linux und `unsigned long` (`CULong`) für
   64-bit OS X)
3. Der `{#fun ... #}` Block ist die c2hs-Anweisung um C-Funktionen für Haskell
   bereit zu stellen. In diesem Fall die C-Funktion `sc_entropy`
4. Die Funktion ist als `pure` deklariert was definiert, dass es sich um eine
   "reine" C-Funktion handelt die also keine Seiteneffekte hat und daher auch
   nicht in Haskells `IO`-Monade laufen muss
5. `unsafe` bedeutet, dass die Funktion ihrerseits keine Haskell-Funktionen
   aufruft, beispielsweise einen Callback.
6. `sc_entropy as ^` sorgt dafür, dass c2hs sich automatisch einen Haskell-Namen
   für die Funktion ausdenkt (hier `scEntropy`)
7. `withByteStringLen *` sorgt dafür, dass für die Konversion von Haskells
   `ByteString` zu `char *` der `withByteStringLen`-Marshaller verwendet wird.
   Der Stern in `withByteStringLen *` deklariert, dass der `withByteStringLen`
   Marshaller in der `IO`-Monade ausgeführt werden muss
8. Das Kaufmannsund (`&`) sorgt dafür, dass dieses Haskell-Argument zu zwei
   C-Funktionsargumenten wird (`char *` und `size_t` in diesem Fall).
9. Für den Rückgabewert (Typ `double` bzw. `Double`) muss kein Marshaller
   angegeben werden, da c2hs für einige Typen Standard-Marshaller kennt und
   diesen anwendet.

Der Haskell-Code für den Marshaller `withByteStringLen` ist oben angegeben. Er
ist hier manuell zu implementieren weil die `shonky-crypt`-Bibliothek das
Argument vom Typ `size_t` (`unsigned int` oder `unsigned long`) erwartet, die
[`unsafeUseAsCStringLen`][bytestring-use-cstrlen] Funktion aus dem
[bytestring][bytestring]-Paket einen `CString` samt Länge aber als `type
CStringLen = (CString, Int)` definiert.

Was natürlich auch noch gesagt werden muss ist, warum hier das scheinbar
unsichere [`unsafeUseAsCStringLen`][bytestring-use-cstrlen] anstatt das sichere
`useAsCStringLen` verwendet wird: Die Bibliothek verspricht durch den Typen
`const char *`, dass sie den Speicher nicht modifizieren wird. Das würde man für
eine solche Funktion natürlich auch erwarten, weil zum Berechnen der Entropie
die Eingabe nicht verändert werden muss. Durch `unsafeUseAsCStringLen` wird also
eine (unnötige) Kopie des ByteStrings verhindert und anstatt dessen direkt der
Zeiger auf den internen Speicher an die C-Bibliothek weitergegeben. Generell
gilt aber natürlich Vorsicht bei der Verwendung von "unsafe"-Funktionen weil
gegenüber der Haskell Laufzeitumgebung eine Garantie ausgesprochen wird.
Sollte das eine nicht zutreffende Garantie sein, ist das Verhalten des Programms
undefiniert.

Damit ist die Arbeit zur Anbindung von `sc_entropy` auch schon getan. C2hs
kompiliert daraus ein Haskell-Modul, was nun die Funktion
`scEntropy :: ByteString -> Double` anbietet. Natürlich war die Anbindung
dieser Funktion denkbar einfach, weil es eine "reine" Funktion
(ohne Seiteneffekte) ist, die dem funktionalen Paradigma entspricht.

### Anbinden von zusammengesetzten C-Datentypen (`struct`) ###

Die Beispielbibliothek exportiert folgende zwei zusammengesetzte Datentypen
(siehe auch [Dokumentation][h]):

    /**
     * A shonky encryption key.
     *
     * key_start:  Each byte is "encrypted" by adding `key_start` (modulo 256) to its
     *             byte value.
     * key_inc:    To "enhance" "encryption", `key_inc` can be used to add an
     *             additional value of `key_inc * n` for the `n`th byte.
     * only_alnum: Only apply the encryption for alphanumeric characters (0-9, a-z,
     *             A-Z). Additionally, the encryption of a byte will stay in its
     *             respective group. In other words, every number's encryption will
     *             be a number, every lower case character's encryption will be
     *             some lower case character and every upper case character will be
     *             encrypted to another upper case character. Non alphanumeric
     *             characters (such as space) will not be encrypted at all.
     */
    typedef struct shonky_crypt_key {
        char key_start;
        char key_inc;
        SC_BOOL only_alnum;
    } *shonky_crypt_key_t;

    struct shonky_crypt_context;
    typedef struct shonky_crypt_context *shonky_crypt_context_t;

Der erste Datentyp (`shonky_crypt_key_t`) ist vom Benutzer der Bibliothek
einsehbar und repräsentiert einen Schlüssel. Der zweite ist ein nicht
einsehbarer Datentyp. Das bedeutet, dass die Bibliothek nach außen nur mit
Zeigern auf Speicher arbeitet, ohne zu verraten, was intern dahintersteckt. Für
den einsehbaren Datentypen macht es Sinn, einen entsprechenden
Haskell-Datentypen zu haben, den man in den C-Datentypen verwandeln kann und
umgekehrt. Wünschenswert wäre also ein Datentyp `data ShonkyCryptKey` welcher
`instance Storable ShonkyCryptKey` erfüllt. Datentypen die
[`Storable`][storable]-Instanzen haben, können in einen Speicherbereich fester
Größe serialisiert und wieder daraus geladen werden. C2hs bietet einige Helfer
an um das Schreiben von `Storable`-Instanzen zu erleichtern.

    data ShonkyCryptKey = ShonkyCryptKey
        { sckKeyStart :: !Word8
        , sckKeyInc :: !Word8
        , sckOnlyAlnum :: !Bool
        } deriving Show

    instance Storable ShonkyCryptKey where
        sizeOf _ = {#sizeof shonky_crypt_key_t #}
        alignment _ = 4
        peek p = ShonkyCryptKey
                   <$> liftM fromIntegral ({#get shonky_crypt_key_t->key_start #} p)
                   <*> liftM fromIntegral ({#get shonky_crypt_key_t->key_inc #} p)
                   <*> liftM toBool ({#get shonky_crypt_key_t->only_alnum #} p)
        poke p x =
          do {#set shonky_crypt_key_t.key_start #} p (fromIntegral $ sckKeyStart x)
             {#set shonky_crypt_key_t.key_inc #} p (fromIntegral $ sckKeyInc x)
             {#set shonky_crypt_key_t.only_alnum #} p (fromBool $ sckOnlyAlnum x)

    {#pointer shonky_crypt_key_t as ShonkyCryptKeyPtr -> ShonkyCryptKey #}

`{#get ...}` bietet Hilfe beim Zugriff auf Elemente von C-`struct`s und `{#set
...}` hilft beim Setzen von Werten in zusammengesetzte C-Datentypen. So wird die
Haskell-Anbindung sehr einfach. `{#pointer ...}` teilt c2hs einen Zusammenhang
zwischen einem Haskell-Typen und einem C-Pointer mit.

Nun besteht also die Möglichkeit mittels der von `Foreign.Marshal.Utils`
bereitgestellten Funktion [`with`][with] ([`with :: Storable a => a -> (Ptr a
-> IO b) -> IO b`][with]) den Haskell-Datentypen `ShonkyCryptKey` zu einem
Zeiger auf eine C-Struktur zu verwandeln und damit C-Funktionen aufzurufen, die
einen Parameter vom Typen `shonky_crypt_key_t` erwarten.


## Anbinden von Funktionen die Zeiger auf allozierten Speicher zurückgeben

Um nun den Marshaller für `ShonkyCryptKey` einsetzen zu können, bietet sich
folgende Funktion der C-Bibliothek an:

    shonky_crypt_context_t sc_alloc_context_with_key(const shonky_crypt_key_t key);

Laut [Dokumentation][h] der C-Bibliothek kann man mit dieser Funktion unter
Angabe eines `shonky_crypt_key_t` einen durch die Bibliothek allozierten
`shonky_crypt_context_t` generieren.  Dank der [`Storable`][storable]-Instanz
ist die Generierung eines `shonky_crypt_key_t`s einfach, allerdings ist der
Rückgabetyp `shonky_crypt_context_t` bis jetzt noch unbenutzbar. Da die
Datenstruktur aber ohnehin von außen nicht einsehbar ist, gibt es auch nicht
viel an Haskell anzubinden. Es genügt, c2hs mitzuteilen, dass es sich um einen
Zeigertypen handelt und c2hs zu bitten, dazu einen entsprechenden Haskell-Typen
bereitzustellen:

    withShonkyCryptContext :: ShonkyCryptContext -> (Ptr ShonkyCryptContext -> IO b) -> IO b
    {#pointer shonky_crypt_context_t as ShonkyCryptContext foreign newtype #}

C2hs generiert dafür einen Haskell-Typen `ShonkyCryptContext`, der als einziges
Feld einen [`Foreign.ForeignPtr`][foreign-ptr] besitzt. Damit lässt nicht sehr
viel mehr tun als einen von der C-Bibliothek erhaltenen Zeiger diesen Types
später nochmals an die C-Bibliothek weiterzureichen. Folgendes Beispiel stellt
`sc_alloc_context_with_key` für Haskell bereit:

    withTrickC2HS :: Storable a => a -> (Ptr a -> IO b) -> IO b
    withTrickC2HS = with

    {#fun pure unsafe sc_alloc_context_with_key as
        ^ { withTrickC2HS *`ShonkyCryptKey' }
        -> `ShonkyCryptContext' newShonkyCryptContextPointer * #}

Nun wieder der Reihe nach:

1. `withTrickC2HS` ist eine simple Umbenennung von [`with`][with], da c2hs
   `with` an verschiedenen Stellen als ein reserviertes Schlüsselwort ansieht,
   was zu Problemen wärend des Parsens der c2hs-Direktiven führt
2. `withTrickC2HS` (aka [`with`][with]) ist der Marshaller für alle Typen die
   eine [`Storable`][storable]-Instanz besitzen, also auch `ShonkyCryptKey`
   (siehe oben)
3. `pure unsafe` ist wie bei der Entropie-Funktion die Garantie, dass diese
   C-Funktion keine Seiteneffekte hat und keine Haskell-Funktionen aufruft.
4. Der Rückgabewert-Marshaller `newShonkyCryptContextPointer` wird benötigt,
   weil die C-Bibliothek laut [Dokumentation][h] einen Zeiger auf in der
   Funktion allozierten Speicher zurück gibt und der Aufrufer nun verantwortlich
   ist ihn nach Benutzung wieder freizugeben. Der Stern bedeutet wieder, dass
   der Marshaller in der `IO`-Monade abzulaufen hat

Um also keine Speicherlecks zu generieren, muss mittels der
Haskell-Lautzeitumgebung also dafür gesorgt werden, dass der Garbage Collector auch
Werte vom Typen `ShonkyCryptContext` wieder korrekt freigibt. Leider dokumentiert die
C-Bibliothek hier auch, dass das mittels der Funktion `sc_release_context()` zu
geschehen hat und nicht etwa mit `free()`. Der `newShonkyCryptContextPointer`
Marshaller muss also einen [`ForeignPtr`][foreign-ptr] zurückgeben, der einen Deallokator
registriert hat, der die C-Funktion `sc_release_context` aufruft.

Das klingt kompliziert und in der Tat hilft c2hs hier nur bedingt weiter. Um
eine Deallokator für [`ForeignPtr`][foreign-ptr] zur registrieren, wird ein
Zeiger auf die Deallokationsfunktion erwartet. Mittels roher Haskell FFI ist
das dennoch nicht zu kompliziert:

    foreign import ccall "shonky-crypt.h &sc_release_context"
      scReleaseContextPtr :: FunPtr (Ptr ShonkyCryptContext -> IO ())

`scReleaseContextPtr` ist nun also ein Funktionszeiger auf `sc_release_context`.
Damit lässt sich dann auch der Marshaller `newShonkyCryptContextPointer` schreiben,
welcher einen Zeiger auf einen ShonkyCryptContext nimmt und ihn mit einem
passenden Deallokator ausstattet.


    newShonkyCryptContextPointer :: Ptr ShonkyCryptContext -> IO ShonkyCryptContext
    newShonkyCryptContextPointer p =
        do fp <- newForeignPtr scReleaseContextPtr p
           return $! ShonkyCryptContext fp

Zurück zur Anbindung Funktion `sc_alloc_context_with_key`:

    {#fun pure unsafe sc_alloc_context_with_key as
        ^ { withTrickC2HS *`ShonkyCryptKey' }
        -> `ShonkyCryptContext' newShonkyCryptContextPointer * #}

Damit generiert c2hs nun eine Haskell-Funktion des Tys

    scAllocContextWithKey :: ShonkyCryptKey -> ShonkyCryptContext

Diese Funktion verwandelt automatisch den Schlüssel in einen Zeiger auf die
C-Struktur und gibt nach Aufruf einen Verschlüsselungskontext zurück, der später
noch interessant wird. Außerdem wird dafür gesorgt, dass der Garbage Collector
den zurückgegebenen Speicher wieder freigibt sobald er nicht mehr benötigt wird.

Mit denselben Methoden wie für `sc_alloc_context_with_key` lässt sich auch
direkt eine weitere Funktion der C-Bibliothek anbinden:

    shonky_crypt_context_t sc_copy_context(const shonky_crypt_context_t ctx);

Diese Funktion kopiert einen vorhandenen Verschlüsselungskontext und gibt
einen Zeiger auf einen neuen, bereits allozierten Verschlüsselungskontext zurück.
Die Haskell-Anbindung gelingt wieder mit wenigen Zeilen c2hs:

    {#fun pure unsafe sc_copy_context as
        ^ { withShonkyCryptContext *`ShonkyCryptContext' }
        -> `ShonkyCryptContext' newShonkyCryptContextPointer * #}

Die bereits bekannten Marshaller `withShonkyCryptContext` und
`newShonkyCryptContextPointer` sorgen dafür, dass das richtige vor und nach der
Ausführung der C-Funktion passiert.

## Anbindung der Verschlüsselungsfunktionen

Zur eigentlichen Verschlüsselung bietet die C-Bibliothek folgende Funktion an:

    void sc_encrypt_inplace(shonky_crypt_context_t ctx,
                            const char *in_plain,
                            char *out_scrambled,
                            size_t sz);

Leider passt diese Funktion überhaupt nicht ins funktionale Paradigma und schon
gar nicht zu Haskell, wo man hauptsächlich mit reinen Funktionen arbeitet. Die
Probleme im Detail:

 * Die Funktion hat den Rückgabewert `void` und schreibt einfach in den
   Speicher, auf den `out_scrambled` zeigt
 * Laut [Dokumentation][h] modifiziert die Funktion den übergebenen
   Verschlüsselungskontext `ctx`

Der Grund warum die Funktion den Kontext modifiziert ist recht einfach: Das
bietet eine API an mit der man beispielsweise große Dateien stückweise
verschlüsseln kann. Da die Bibliothek aber laut [Dokumentation][h]
unterstützt, dass der Schlüssel sich fortlaufend verändert, muss sie den Kontext
verändern um jeweils daran erinnert zu werden, wie viele Daten insgesamt bereits
verschlüsselt wurden.

Eine funktionale API für so eine Funktion wäre zum Beispiel

    encryptS :: ShonkyCryptContext -> ByteString -> (ByteString, ShonkyCryptContext)

Übergeben wird ein Eingabe-Kontext und das derzeitige Textstück und zurück kommt
das entsprechende verschlüsselte Textstück und ein neuer Kontext, der unabhängig
vom alten ist. Der Eingabe-Kontext ist also vor Benutzung zu kopieren, damit
die C-Bibliothek den Eingabe-Kontext nicht zerstört, was überhaupt nicht zu
Haskell passen würde, weil es die [Referenzielle
Transparenz](http://de.wikipedia.org/wiki/Referenzielle_Transparenz) verletzen
würde.

Um die Anbindung zu beginnen, wird zunächst ein Haskell-Typ modelliert, der
dieser Art von C-Funktionen entspricht.

    type InPlaceEnDeCryptFun =
         Ptr ShonkyCryptContext -> Ptr CChar -> Ptr CChar -> CSizeT -> IO ()

Natürlich muss das in der `IO` Monade ablaufen, weil die Funktion den Speicher
auf den `Ptr ShonkyCryptContext` zeigt modifizieren wird. Um der C-Funktion
`sc_encrypt_inplace` einen funktionalen Gegenpart zu spendieren, muss etwas
tiefer in die Trickkiste gegriffen werden, die von c2hs bereitgestellte
Funktionalität reicht hier nicht aus. Anstatt dessen wird das mehr manuell
programmiert:

    scEnDeCryptInplace :: InPlaceEnDeCryptFun
                       -> ShonkyCryptContext
                       -> ByteString
                       -> (ByteString, ShonkyCryptContext)
    scEnDeCryptInplace f ctx input =
         unsafePerformIO $
         BSU.unsafeUseAsCStringLen input $ \(inputBytes, inputLen) ->
         withShonkyCryptContext ctx $ \ctx' ->
         do
            newContext' <- {#call sc_copy_context #} ctx'
            outBuffer <- mallocBytes inputLen
            f newContext' inputBytes outBuffer (fromIntegral inputLen)
            out <- unsafePackMallocCStringLen (outBuffer, inputLen)
            newContext <- newShonkyCryptContextPointer newContext'
            return (out, newContext)

    encryptS :: ShonkyCryptContext -> ByteString -> (ByteString, ShonkyCryptContext)
    encryptS = scEnDeCryptInplace {#call unsafe sc_encrypt_inplace #}

Wieder der Reihe nach:

1. Die Funktion `scEnDeCryptInplace` existiert, damit dieser Code nur einmal
   geschrieben werden muss und somit auch direkt für die praktisch gleiche
   Funktion `sc_decrypt_inplace` verwendet werden kann
2. Die C-Funktion `sc_copy_context` wird direkt aufgerufen, damit keine Probleme
   mit der Mutation der Kopie des Eingabe-Kontextes entstehen. Sobald der einmal
   mutierte Kontext dann von Haskell aus bekannt ist, ist garantiert, dass er
   nicht ein weiteres Mal mutiert wird
3. [`unsafePerformIO`][unsafe-perform-io] garantiert dem Haskell-Lautzeitsystem,
   dass die Funktion im ganzen wiederum frei von Seiteneffekten ist. Es ist die
   Aufgabe des Programmierers abzusichern, dass das tatsächlich der Fall ist
4. Das Mashalling wird manuell erledigt
5. Wiederum wird [`unsafeUseAsCStringLen`][bytestring-use-cstrlen] benutzt, weil
   die C-Bibliothek den Eingabe-Text nicht modifiziert
6. Den Ausgabepuffer wird mittels [`mallocBytes`][malloc-bytes] (entspricht
   `malloc()`) alloziert
7. Außerdem wird [`unsafePackMallocCStringLen`][unsafe-pack-malloc-cstrlen]
   benutzt um mittels des mittlerweile von der C-Bibliothek überschriebenen
   Puffers einen Haskell `ByteString` zu konstruieren. Das ist hier möglich,
   weil die C-Bibliothek keinen Zeiger mehr auf diesen Speicher hält, ihn also
   auch in Zukunft nicht mehr modifizieren kann

Die eigentliche Anbindung der C-Funktion `sc_encrypt_inplace` und der entsprechenden Dechiffrierungsfunktion `sc_decrypt_inplace` ist dann sehr einfach:

    encryptS :: ShonkyCryptContext -> ByteString -> (ByteString, ShonkyCryptContext)
    encryptS = scEnDeCryptInplace {#call unsafe sc_encrypt_inplace #}

C2hs stellt für direkte Anbindung `{#call ... #}` bereit, was direkt die
C-Funktion vom Typ `Ptr ShonkyCryptContext -> Ptr CChar -> Ptr CChar -> CSizeT
-> IO ()` bereitstellt. Die Funktion `scEnDeCryptInplace` verwandelt solche
Funktionen dann in Haskell-Funktionen mit annehmbaren funktionalen Interface. Die
entsprechende Entschlüsslungsfunktion ist dann ebenso einzubinden:

    decryptS :: ShonkyCryptContext -> ByteString -> (ByteString, ShonkyCryptContext)
    decryptS = scEnDeCryptInplace {#call unsafe sc_decrypt_inplace #}

# Zusätzliche Haskell-Funktionen

Bisher wurde hauptsächlich die relativ C-nahe Anbindung von Haskell
Funktionen besprochen. Die derzeitigen Möglichkeiten entsprechen den folgenden
Funktionen.

    scEntropy :: ByteString -> Double
    ShonkyCryptKey :: Word -> Word -> Bool -> ShonkyCryptKey
    scAllocContextWithKey :: ShonkyCryptKey -> ShonkyCryptContext
    encryptS :: ShonkyCryptContext -> ByteString -> (ByteString, ShonkyCryptContext)
    decryptS :: ShonkyCryptContext -> ByteString -> (ByteString, ShonkyCryptContext)

Mit diesen Funktionen lässt sich die Verschlüsselungsbibliothek schon vollständig
nutzen, da Haskell seine Stärken aber besonders in der
"high-level"-Programmierung ausspielen kann soll das hier auch in kleinen
Beispielen demonstriert werden. Auf die Haskell-Implementierung der
"high-level"-Funktionen wird hier der Übersicht wegen verzichtet, es
werden nur die jeweiligen Funktions-Prototypen vorgestellt. Natürlich ist die
vollständige Implementierung im [GitHub-Projekt][github] (im Modul
`Codec.ShonkyCrypt` [(Haskell Quelldatei)][hs]) zu finden. Ebenso findet sich
dort die vollständige Anbindung der C-nahen Funktionen im Modul
`Codec.ShonkyCrypt.ShonkyCryptFFI` ([c2hs Quelldatei][chs]).

Die Haskell-Funktionen gehen über den Funktionsumfang der C-Version hinaus
und weil das Schreiben von Unit-Tests in Haskell so viel Spaß macht sind sowohl
die C-nahen als auch die "high-level"-Funktionen im Modul
[`test/TestShonkyCrypt.hs`][test-hs] getestet.

Folgende in Haskell implementierten Funktionen sind im [Beispiel-Modul][hs]
implementiert:

    caesar :: Text -> Text
    encryptM :: MonadState ShonkyCryptContext m => ByteString -> m ByteString
    encryptConduit :: MonadResource m => ShonkyCryptKey -> Conduit ByteString m ByteString

Die Funktion `caesar` führt eine "Cäsar-Verschlüsselung" (Rotation um 3
Buchstaben) auf `Text` (Encoding ist UTF-8) durch, was nett zum Ausprobieren der
Bibliothek ist

    $ ghci -XOverloadedStrings dist/build/src-c/shonky-crypt/shonky-crypt.o
    dist/build/src-c/shonky-crypt/entropy.o
    *Codec.ShonkyCrypt Codec.ShonkyCrypt> caesar "HELLO WORLD!"
    "KHOOR ZRUOG!"

Die Funktion `encryptM :: MonadState ShonkyCryptContext m => ByteString -> m
ByteString` stellt eine monadische Version bereit, die den
Verschlüsselungskontext in einer `MonadState` hält. Und zu guter letzt
`encryptConduit :: MonadResource m => ShonkyCryptKey -> Conduit ByteString m
ByteString`, ein "Conduit" für das
[`conduit`](http://hackage.haskell.org/package/conduit-1.0.7.3)-Paket:

    > runResourceT $ sourceFile "/etc/hosts" $= encryptConduit (ShonkyCryptKey 3 0 True) $$ consume
    ["##\n# Krvw Gdwdedvh\n#\n# orfdokrvw lv xvhg wr frqiljxuh wkh orrsedfn lqwhuidfh\n# zkhq wkh vbvwhp lv errwlqj.  Gr qrw fkdqjh wklv hqwub.\n##\n450.3.3.4\torfdokrvw\n588.588.588.588\teurdgfdvwkrvw\n::4             orfdokrvw \nih13::4%or3\torfdokrvw\n"]

# Ausprobieren

Um das ganze selbst auszuprobieren, sollten die folgenden Kommandos ausreichen.
Cabal übernimmt dann den c2hs-Schritt, das Kompilieren der C-Bibliothek, sowie
das kompilieren der Haskell-Bibliothek. Cabal unterstützt c2hs direkt, ein
`build-tools: c2hs -any` in der `.cabal`-Datei genügt damit Cabal Haskell-Module
automatisch mit c2hs vorverarbeitet falls nötig.

    git clone https://github.com/weissi/hs-shonky-crypt.git
    cd hs-shonky-crypt
    cabal install --only-dependencies          # Abhängigkeiten installieren
    cabal install c2hs                         # C2hs Präprozessor installieren
    cabal configure --enable-tests             # Mit Unit-Tests kompilieren
    cabal build                                # Kompilieren
    dist/build/TestShonkyCrypt/TestShonkyCrypt # Unit-Tests ausführen
    cabal test                                 # (ohne schöne Ausgabe)

[hs]: https://github.com/weissi/hs-shonky-crypt/blob/master/src-hs/Codec/ShonkyCrypt.hs
[test-hs]: https://github.com/weissi/hs-shonky-crypt/blob/master/test/TestShonkyCrypt.hs
[chs]: https://github.com/weissi/hs-shonky-crypt/blob/master/src-hs/Codec/ShonkyCrypt/ShonkyCryptFFI.chs
[h]: https://github.com/weissi/hs-shonky-crypt/blob/master/src-c/shonky-crypt/shonky-crypt.h
[shannon]: http://de.wikipedia.org/wiki/Entropie_%28Informationstheorie%29
[bytestring-use-cstrlen]: http://hackage.haskell.org/packages/archive/bytestring/latest/doc/html/Data-ByteString-Unsafe.html#v:unsafeUseAsCStringLen
[bytestring]: http://hackage.haskell.org/package/bytestring
[with]: http://hackage.haskell.org/packages/archive/base/4.6.0.1/doc/html/Foreign-Marshal-Utils.html#v:with
[unsafe-perform-io]: http://hackage.haskell.org/packages/archive/base/4.6.0.1/doc/html/System-IO-Unsafe.html#v:unsafePerformIO
[malloc-bytes]: http://hackage.haskell.org/packages/archive/base/4.6.0.1/doc/html/Foreign-Marshal-Alloc.html#v:mallocBytes
[unsafe-pack-malloc-cstrlen]: http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Unsafe.html#v:unsafePackMallocCStringLen
[storable]: http://hackage.haskell.org/packages/archive/base/4.6.0.1/doc/html/Foreign-Storable.html#t:Storable
[foreign-ptr]: http://hackage.haskell.org/packages/archive/base/4.6.0.1/doc/html/Foreign-ForeignPtr-Safe.html#t:ForeignPtr
[github]: https://github.com/weissi/hs-shonky-crypt
[c2hs-guide]: https://github.com/haskell/c2hs/wiki/User-Guide
[c2hs-wiki]: https://github.com/haskell/c2hs/wiki

<!-- more end -->
