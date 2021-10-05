import itertools

alpha = 'abcdefghijklm'

def make_signature(count):
    intype = "(("
    outtype = "t<("
    for i in range(count):
        param = "'" + alpha[i]
        intype += f"t<{param}>"
        outtype += param
        if i < count-1:
            intype += ", "
            outtype += ", "
    intype += "))"
    outtype += ")>"
    return f"let both{count}: {intype} => {outtype}"


def all_ok(count):
    arg = []
    res = []
    for i in range(count):
        param = alpha[i]
        arg.append(f"Ok({param})")
        res.append(param)
    arg = ", ".join(arg)
    res = ", ".join(res)
    return f"  | ({arg}) => Ok(({res}))"

def have_errs_1(count):
    result = []
    for i in range(count):
        positions = ["_"] * count
        positions[i] = "Err(e)"
        result.append(f"  | ({', '.join(positions)}) => Err(e)")
    return result


def have_errs_2(count):
    result = []
    for i in range(count):
        for j in range(i+1, count):
            positions = ["_"] * count
            positions[i] = f"Err(e{i+1})"
            positions[j] = f"Err(e{j+1})"
            out = f"Err(Error.join(e{i+1}, e{j+1}))"
            result.append(f"  | ({', '.join(positions)}) => {out}")
    return result


def have_errs(count, errs):
    if errs == 1:
        return have_errs_1(count)
    if errs == 2:
        return have_errs_2(count)
    result = []
    for pattern in itertools.combinations(range(count), errs):
        positions = ["_"] * count
        for i in pattern:
            positions[i] = f"Err(e{i+1})"
        out = ", ".join(f"e{i+1}" for i in pattern)
        out = "Err(Error.concat(list{" + out + "}))"
        result.append(f"  | ({', '.join(positions)}) => {out}")
    return result


def make_code(count):
    defline = f"let both{count} = ts =>"
    switchline = "  switch ts {"
    closeline = "  }"
    matchlines = [all_ok(count)]
    for i in range(count, 0, -1):
        matchlines.extend(have_errs(count, i))
    return "\n".join([defline, switchline, *matchlines, closeline])

MAX = 13+1

for i in range(3, MAX):
    print(make_signature(i))
    # print(make_code(i))
