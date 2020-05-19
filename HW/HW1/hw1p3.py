def pareseInput(firstLine, secondLine):
    l1 = len(firstLine)
    l2 = len(secondLine)

    if l1 == 0:
        return False

    if l1 != 0 and l2 != 0:
        firstChar = firstLine[0]

        if not (firstChar == '\'' or firstChar == '\"'):
            return False
        lastChar = secondLine[-1]

        if firstChar != lastChar:
            return False

        # validate 1st string
        for index in range(1, len(firstLine)):
            if firstLine[index] == firstChar and firstLine[index - 1] != '\\':
                return False

        count = 0
        for index in range(len(firstLine) - 1, 0, -1):
            if firstLine[index] == '\\':
                count += 1
            else:
                break
        if count % 2 == 0:
            return False

        # validate 2nd string
        for index in range(0, len(secondLine) - 1):
            if index == 0 and secondLine[index] == '\\':
                continue
            elif secondLine[index] == firstChar and secondLine[index - 1] != '\\':
                return False

        count = 0
        for index in range(len(secondLine) - 2, 0, -1):
            if secondLine[index] == '\\':
                count += 1
            else:
                break
        if count % 2 != 0:
            return False

        return True

    elif l1 != 0 and l2 == 0:
        firstChar = firstLine[0]

        if not (firstChar == '\'' or firstChar == '\"'):
            return False
        lastChar = firstLine[-1]
        if firstChar != lastChar:
            return False

        # validate 1st string
        for index in range(1, len(firstLine) - 1):
            if firstLine[index] == firstChar and firstLine[index - 1] != '\\':
                return False

        count = 0
        for index in range(len(firstLine) - 2, 0, -1):
            if firstLine[index] == '\\':
                count += 1
            else:
                break
        if count % 2 != 0:
            return False

        return True
    else:
        return False


firstLine = input("Enter 1st line: ")
secondLine = input("Enter 2nd line: ")

firstLine = firstLine.strip(" ")
secondLine = secondLine.rstrip(" ")
print(str(pareseInput(firstLine, secondLine)))

# x = [
#     ("\'Hello\\", "World\\\\\'"),
#     ('\\', "\"Hello\""),
#     ("\'Hello\'", ''),
#     ('\\', "\'shouldn\\\'t\\\\\'"),
#     ('\"hello\\', "world'"),
#     ("\"hello\\\\", "world\"")
# ]
#
# for each in x:
#     firstLine = each[0]
#     secondLine = each[1]
#
#     firstLine = firstLine.strip(" ")
#     secondLine = secondLine.rstrip(" ")
#
#     print(str(pareseInput(firstLine, secondLine)))
