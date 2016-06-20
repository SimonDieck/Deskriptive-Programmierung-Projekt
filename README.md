# Deskriptive-Programmierung-Projekt

In meinem Projekt möchte ich die Regeltexte von "Magic: The Gathering"-Karten so parsen, dass sie direkt auf ein von mir erstelltes Modell zur Darstellung eines Spielzustandes angewendet werden können.
Als Grundlage verwende ich dafür die Edition "Magic2010", auf die ich in Json-Format von http://mtgjson.com/ zugreife.
Das Projekt soll in Haskell realisiert werden und beinhaltet zwei größere Schritte:

1. Erstellung eines Modells zur eindeutigen Beschreibung eines Spielzustandes in "Magic: The Gathering"
2. Erstellung eines Parsers, der Regeltexte von Karten in eine Datenstruktur parsed, aus der Veränderungen des Modells direkt ablesbar sind.

