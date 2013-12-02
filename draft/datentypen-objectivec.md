---
layout: post
title: "Einfache Datentypen mit Objective-C"
author: stefan-wehr
tags: ["Objective-C", "Datentypen"]
---

Heute geht's mal wieder darum zu zeigen, dass sich die Beschäftigung mit funktionaler 
Programmierung auch dann lohnt, wenn man oft in imperativen oder objekt-orientierten
Sprache unterwegs ist. Der Artikel zeigt anhand eines Beispiels aus der Praxis,
wie wir in einer in Objective-C geschriebenen iOS-App Einflüsse aus der funktionalen
Programmierung aufgenommen und damit eine sehr einfache Datenmodellierung
ermöglicht haben. Wenn Sie selbst auch mit Objective-C programmieren, können
Sie unsere Ergebnisse direkt nutzen, denn sie stehen auf github unter einer
Open-Source-Lizenz [zur Verfügung](https://github.com/skogsbaer/magic-property).


<!-- more start -->

Datenmodellierung in funktionalen Sprachen ist sehr einfach: man definiert
auf ganz natürliche Art und Weise einen oder mehrere Datentypen mit den gewünschten
Feldern, falls nötig kann man auch für einen Datentypen mehrere Alternativen
festlegen. Betrachten wir als Beispiel eine einfache Modellierung eines Telefonbuch-Eintrags
in Haskell. Der Einfachheit halber verzichten wir in diesem Beispiel
auf Datentypen mit Alternativen.

{% highlight haskell %}
data PhonebookEntry
   = PhonebookEntry
     { name         :: String
     , email        :: String
     , phoneNumbers :: [String]  
     , address      :: Address 
     }
   deriving (Show, Eq)

data Address
   = Address
   { street   :: String
   , city     :: String
   , country  :: String
   }
   deriving (Show, Eq)
{% endhighlight %}


Einfach, oder? Der `deriving (Show, Eq, Ord)` Teil sorgt dafür, dass der Compiler
automatisch Funktionen zur Umwandlung eines Wert des Datentypens in einen
String (`show`) und zum Test auf Gleichheit (`==`) generiert. Eine weitere
schöne Eigenschaft der obigen Datentypen ist, dass alle Werte automatisch
unveränderbar sind. (Auf englisch heißt das dann "immutable". Auch die
objekt-orientierten Programmieren wissen diese Eigenschaften zu schätzen,
wie etwa Joshua Bloch in seinem Buchklassiker "Effective Java" demonstriert.)

Was müssen wir tun, um dieselbe Funktionalität in einer objekt-orientieren Sprache
wie Objective-C zu implementieren? Hier ist meine Version, dabei verzichte
ich auf Hinschreiben der Implementierung von `Address`.

{% highlight objective-c %}
// phonebookentry.h

#import <Foundation/Foundation.h>
#import "address.h"

@interface PhonebookEntry : NSObject
// Eigenschaften
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, copy, readonly) NSString *email;
@property (nonatomic, copy, readonly) NSArray *phoneNumbers;
@property (nonatomic, copy, readonly) Address *address;

// Konstruktor
- (id)initWithName:(NSString *)name 
             email:(NSString *)email
      phoneNumbers:(NSArray *)phoneNumbers
           address:(Address *)address;

// Methoden zum Kopieren
- (PhonebookEntry *)copyWithName:(NSString *)name;
- (PhonebookEntry *)copyWithEmail:(NSString *)email;
- (PhonebookEntry *)copyWithPhoneNumbers:(NSArray *)phoneNumbers;
- (PhonebookEntry *)copyWithAddress:(Address *)address;
@end

// phonebookentry.m

#import "phonebookentry.h"

// Erneute Deklaration der Properties um eine private, schreibbare
// Sicht zu haben.
@interface PhonebookEntry ()
@property (nonatomic, copy, readwrite) NSString *name;
@property (nonatomic, copy, readwrite) NSString *email;
@property (nonatomic, copy, readwrite) NSArray *phoneNumbers;
@property (nonatomic, copy, readwrite) Address *address;
@end

@implementation PhonebookEntry

- (id)initWithName:(NSString *)name 
             email:(NSString *)email
      phoneNumbers:(NSArray *)phoneNumbers
           address:(Address *)address
{
    if ((self = [super init])) {
        self.name = name;
        self.email = email;
        self.phoneNumbers = phoneNumbers;
        self.address = address;
    }
    return self;
}

- (BOOL)isEqual:(id)other
{
    // Implementierung nicht abgedruckt. Die Implementierung ist
    // aber nicht trivial. Es gibt mehrere Fallen, vergleiche auch
    // das "Effective Java" Buch von Joshua Bloch
}

- (NSUInteger)hashCode
{
    // Implementierung nicht abgedruckt, aber auch nicht trivial
}

- (NSString *)description
{
    // Implementierung nicht abgedruckt.
}

- (PhonebookEntry *)copyWithName:(NSString *)name
{
    // Implementierung nicht abgedruckt.
}

- (PhonebookEntry *)copyWithEmail:(NSString *)email
{
    // Implementierung nicht abgedruckt.
}

- (PhonebookEntry *)copyWithPhoneNumbers:(NSArray *)phoneNumbers
{
    // Implementierung nicht abgedruckt.
}

- (PhonebookEntry *)copyWithAddress:(Address *)address
{
    // Implementierung nicht abgedruckt.
}
@end
{% endhighlight %}

Verdammt viel Code, oder? Und ich bin mir sicher, eine vergleichbare Implementierung
etwa in Java wäre nicht wesentlich kürzer.

Solche Art von Boilerplate-Code hat mehrere Nachteile. Man verschwendet viel
Zeit um den Code hinzuschreiben, was unserer Erfahrung nach häufig zu einer unsauberen
Datenmodellierung führt. Wir haben nämlich festgestellt, dass wir in unseren iOS-Projekt
aus Gründen der Zeitersparnis häufig darauf verzichten die einzelnen Properties
nach außen hin nicht schreibbar zu machen, dass wir `isEqual:` und `hashCode` nicht
überschreiben, oder dass wir keine sinnvolle `description` Methode implementieren.

Da solche "Schlampigkeiten" früher oder später zu Problemen führen, haben wir ein
kleines Codegenerierungsskript geschrieben, welches uns das Schreiben des ganzen Boilerplate-Codes
abnimmt. Mit dieser Codegenerierung reduziert sich das obige Beispiel auf folgende Zeilen,
der Rest wird generiert.

{% highlight objective-c %}
#import <Foundation/Foundation.h>
#import "Address.h"

@interface PhonebookEntry : NSObject
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, copy, readonly) NSString *email;
@property (nonatomic, copy, readonly) NSArray *phoneNumbers;
@property (nonatomic, copy, readonly) Address *address;
@end
{% endhighlight %}

Einfach, oder? 

Diese Codegenerierung steht auch der Öffentlichkeit zur Verfügung, und
zwar als github Repository [https://github.com/skogsbaer/magic-property](https://github.com/skogsbaer/magic-property).
Dort wird auch erklärt, wie sie die Codegenerierung in ihr Projekt einbinden.
