import re


def typeOfNumber(inputStr):
    global index
    length = len(inputStr)

    # remove initial '+' or '-'
    # if you find anything else return None
    for index in range(length):
        if inputStr[index] == '+' or inputStr[index] == '-':
            # You can't have both '+' and '-'
            continue
        elif '0' <= inputStr[index] <= '9' or inputStr[index] == ".":
            break
        else:
            return "None"

    modifiedInputStr = inputStr[index:]

    # Integer
    if re.fullmatch('^[1-9][0-9]*$', modifiedInputStr):
        return "int"
    # Integer
    if re.fullmatch('^[0]+$', modifiedInputStr):
        return "int"
    # Binary
    elif re.fullmatch('^0[bB][0-1]+$', modifiedInputStr):
        return "int"
    # Octal
    elif re.fullmatch('^0[oO][0-7]+$', modifiedInputStr):
        return "int"
    # Hexadecimal
    elif re.fullmatch('^0[xX][a-fA-F0-9]+$', modifiedInputStr):
        return "int"
    # floating point
    elif re.fullmatch('^[0-9]*[.][0-9]*$', modifiedInputStr):
        return "float"
    # Scientific Notation
    elif re.fullmatch('^[0-9]*[.]?[0-9]*[eE][+-]?[0-9]*$', modifiedInputStr):
        return "float"
    else:
        return "None"


inputStr = input("Enter number: ")
print(typeOfNumber(inputStr))

# testArray = ["++12", "0xc2", "000.333", "5.2e10", "01", "0xacz", "0b00111", "0o05", "0xaa", "0b", "+++10.1", "12a",
#              "05.2", "1e10", "-.3"]
# for index in range(len(testArray)):
#     print(testArray[index] + " " + typeOfNumber(testArray[index]))
