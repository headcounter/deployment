#!/usr/bin/env python3


def mkreplace(char):
    b = bytes([char])
    if b == b"\"":
        inb = b"\\\""
        outb = b"\\\\\\\""
    elif b == b"\n":
        inb = b"\\n"
        outb = b"\\\\n"
    elif b == b"\r":
        inb = b"\\r"
        outb = b"\\\\r"
    elif b == b"\t":
        inb = b"\\t"
        outb = b"\\\\t"
    elif b == b"\\":
        inb = b"\\\\"
        outb = b"\\\\\\\\"
    else:
        inb = b
        outb = b"\\\\x" + b.hex().encode('ascii')
    return (b"\"" + inb + b"\"", b"\"" + outb + b"\"")


def write_replacer(what, where):
    where.write(b"\t")
    for n, item in enumerate(what):
        if n % 9 == 0:
            if n > 0:
                where.write(b"\n\t")
        where.write(item)
        if (n + 1) % 9 != 0 and n != len(what) - 1:
            where.write(b"\t")
    where.write(b"\n")


with open('escape-string.nix', 'wb') as cf:
    cf.write(b"# vi:ts=8:\n")
    crange = list(range(1, 32)) + [34, 92] + list(range(127, 256))
    rep_from, rep_to = zip(*[mkreplace(i) for i in crange])
    cf.write(b"builtins.replaceStrings [\n")
    write_replacer(rep_from, cf)
    cf.write(b"] [\n")
    write_replacer(rep_to, cf)
    cf.write(b"]\n")
