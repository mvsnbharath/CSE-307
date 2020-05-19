def isIdentifier(inputStr):
    length = len(inputStr)

    # Empty String
    if length == 0:
        return False

    keywordSet = {'False', 'class', 'from', 'or',
                  'None', 'continue', 'global', 'pass',
                  'True', 'def', 'if', 'raise',
                  'and', 'del', 'import', 'return',
                  'as', 'elif', 'in', 'try',
                  'assert', 'else', 'is', 'while',
                  'async', 'except', 'lambda', 'with',
                  'await', 'finally', 'nonlocal', 'yield',
                  'break', 'for', 'not'
                  }

    if inputStr in keywordSet:
        return False

    # check for 1st character
    if not (('a' <= inputStr[0] <= 'z') or ('A' <= inputStr[0] <= 'A') or (inputStr[0] == '_')):
        return False

    for i in range(1, length):
        if not (('a' <= inputStr[i] <= 'z') or ('A' <= inputStr[i] <= 'Z') or ('0' <= inputStr[i] <= '9') or (inputStr[i] == '_')):
            return False
    return True


inputStr = input("Enter identifier: ")
print(isIdentifier(inputStr))

# check=["___","_while","while","12","abc!"]
# for word in check:
#     print(isIdentifier(word))


