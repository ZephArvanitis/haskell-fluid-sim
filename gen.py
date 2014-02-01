
EDGE_OFFSET_DATA = {
        0 :         ("-y", "-z"),
        1 :         ("+x", "-z"),
        2 :         ("+y", "-z"),
        3 :         ("-x", "-z"),
        4 :         ("-y", "+z"),
        5 :         ("+x", "+z"),
        6 :         ("+y", "+z"),
        7 :         ("-x", "+z"),
        8 :         ("-x", "-y"),
        9 :         ("+x", "-y"),
        10:         ("+x", "+y"),
        11:         ("-x", "+y"),
}

def offset_gen(one, two):
    pm1 = 1 if one[0] == "+" else -1
    pm2 = 1 if two[0] == "+" else -1
    d1 = one[1]
    d2 = two[1]
    dirs = {
            "x" : (1, 0, 0),
            "y": (0, 1, 0),
            "z": (0, 0, 1)
    }
    a = [0, 0, 0]
    b = map(lambda x: x * pm1, dirs[d1])
    c = map(lambda x: x * pm2, dirs[d2])
    d = [x + y for x, y in zip(b, c)]
    return map(tuple, [a, b, c, d])

def print_offset_dirs():
    for ind, dirs in DATA.items():
        print "{",
        for x in offset_gen(*dirs):
            print x, ",",
        print "},"

print_offset_dirs()
