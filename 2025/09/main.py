
if __name__ == "__main__":
    with open("input.txt", "r") as file:
        content = file.read()
    lines = content.split('\n')
    points = [line.split(',') for line in lines if line != '']
    maxArea = 0
    for p in points:
        for q in points:
            px = int(p[0])
            py = int(p[1])
            qx = int(q[0])
            qy = int(q[1])
            maxArea = max(maxArea, abs(px - qx + 1) * abs(py - qy + 1))
    print(maxArea)
