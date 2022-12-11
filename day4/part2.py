from sys import argv

def parse(lines):
    def chunks(l):
        data = l.split(',')
        if len(data) != 2:
            raise "Wrong number of segments (expected 2)"
        return data
    def to_range(r):
        parts = r.split('-')
        if len(parts) != 2:
            raise "Wrong number of range parts (expected 2)"
        start, end = parts
        return range(int(start), int(end) + 1) # range is [), we want []

    return map(lambda l: map(to_range, chunks(l)), lines) 

def count_overlaps(data):
    def range_contains(a, b):
        return len(set(a) & set(b)) > 0

    def either_contains(a, b):
        return range_contains(a, b) or range_contains(b, a)
    return sum(either_fully_contains(fst, snd) for fst, snd in data)

if __name__ == '__main__':
    with open(argv[1]) as file:
        data = file.readlines()
        print(count_overlaps(parse(data)))
