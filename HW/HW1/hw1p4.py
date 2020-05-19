import datetime


def parseDate(date_string):
    date_list = date_string.split('/')

    if not (len(date_list) == 3 and len(date_list[0]) == 2 and len(date_list[1]) == 2 and len(date_list[2]) == 4):
        return None

    try:
        date_obj = datetime.datetime.strptime(date_string, '%m/%d/%Y')
        return date_obj.strftime('%A') + ", " + date_obj.strftime('%B') + str(" ") + str(date_obj.day) + ", " + str(
            date_obj.year)
    except ValueError:
        return None


# check = ["02/07/2019", "15/15/1945", "03/12/0200", "aa"]
# for word in check:02/07/2019
#     print(parseDate(word))

inputStr = input("Enter date: ")
print(parseDate(inputStr))
