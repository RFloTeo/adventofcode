import math

def qsort(l):
    if len(l) <= 1:
        return l

    mid = l[0]
    left = [x for x in l[1:] if x <= mid]
    right = [x for x in l[1:] if x > mid]
    sorted_l = qsort(left)
    sorted_r = qsort(right)
    return sorted_l + [mid] + sorted_r

def part_one(left, right):
    ll = qsort(left)
    rr = qsort(right)

    length = len(ll)
    answer = 0
    for n in range(length):
        answer += abs(ll[n] - rr[n])

    print(answer)

def part_two(left, right):
    freq = {}
    for n in right:
        if n in freq:
            freq[n] += 1
        else:
            freq[n] = 1

    similarity = 0
    for n in left:
        if n in freq:
            similarity += n * freq[n]
    print(similarity)

f = open("in.txt", "r")
lines = f.readlines()
f.close()

left = []
right = []
for line in lines:
    nos = line.split()
    left.append(int(nos[0]))
    right.append(int(nos[1]))

# part_one(left, right)
part_two(left, right)
