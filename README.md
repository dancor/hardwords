A very-high-entropy password manager using 3 and 4 letter words and scrypt.
Many of the 3 and 4 letter words are very obscure, but these passwords are
very typeable, quite short, and fairly memorable (particularly if you like word
games or obscure english words) given their very high entropy.

The "master" passwords are 23 letters (7 words) and have 80 bits of
entropy. The web-friendly "derived" passwords are 17 letters (5 words)
with 59 bits of entropy. For comparison, the
xkcd example has a whopping 25 characters for only 44 bits of entropy,
but is much easier to memorize.

## example use
- Generating a master password initially:
```shell
~ ./hardwords master
cherapslagjolvinyodstor
CHE dialect version of I, also CH [pron]
RAP to strike sharply [v RAPPED, RAPPING, RAPS]
SLAG to criticize, mock or deride [v SLAGGED, SLAGGING, SLAGS]
JOL to have a good time (S African Slang), JOLLED, JOLLING, JOLS [v -ED, -ING, -S]
VINY covered with vines [adj VINIER, VINIEST]
ODS <od=n> [n]
TOR a high, craggy hill [n -S]
```
- Day-to-day use for derived passwords:
```shell
~ ./hardwords example.com
Enter hardwords master password: [user enters cherapslagjolvinyodstor]
jell1Yebocolhomcru
- JELL to {congeal=v} [v -ED, -ING, -S]
- YEBO yes [interj]
- COL a depression between two mountains [n -S]
- HOM a sacred plant of the ancient Persians, also HOMA [n -S]
- CRU a {vineyard=n} [n -S]
```

## references
- https://xkcd.com/936
- https://en.wikipedia.org/wiki/Scrypt

## dependencies
- You need ghc and the haskell package "scrypt". scrypt-0.5.0 was current at this writing.

## installation
```shell
~ sudo cp dict/csw3 dict/csw4 /usr/share/dict
~ make
```

## competitors
- https://xkpasswd.net/s/
  - By default, you still have to memorize numbers and symbols and only uses 3
    long words.
    Many short words with a minimum of other stuff is the best tradeoff for
    security, typeability, and memorization.
    This site does tell you entropy bits which is good
    (the "full knowledge" one counts), and you can massage the many settings
    to some extent. Actually all of these have competitors have similar issues.
- https://github.com/redacted/XKCD-password-generator
- http://correcthorsebatterystaple.net/
- http://vakila.github.io/rc-projects/xkcd-pass/
- https://www.schneier.com/blog/archives/2014/03/choosing_secure_1.html
  - This seems fairly good if you can't use a password generator program
    and instead you have to generate your password totally on your own.
    However, it's really hard to determine the exact entropy of the result.
    It's not hard to imagine that the entropy could be quite low and that
    this could be attacked, in our age of Deep Learning, if not more naively.
    Word-list-based password generation gives an exact and provable entropy
    level.
  - Having one master password may be more user friendly than this system of
    all totally separate passwords. Actually this applies to every system here.

## todo
- Reconcile the 80-bit/59-bit discepancy; actually create more options so
  that people can get the particular ease/security trade-off they want?
- Switch from scrypt to cryptonite since that's more readily available?
  It has an Ubuntu package for example.
- Switch to the Enable wordlist?
- Hard code the dictionaries into the program so the executable is fully
  standalone?
- Make a more user-friendly mode with more common words, allowing longer
  words?
- Setting just to show the mnemonics and not the constructed password?
  Just for a bit of over-the-shoulder protection (well, obscurity security).
- Setting to copy to clipboard? Should have a time limit?
- Nonconsole version?
