import requests
import json
file = open("corpus.txt",'rb')
text = file.read()
#take response from sever Duckling
Data = {'locale':'ru_RU','text':text}
url='http://0.0.0.0:8000/parse'
response = requests.post(url,data=Data)
text=response.text

f= open("output.txt",'w')
f.write(text)
with open('output.json', 'w') as outfile:
    outfile.write(str(response.json()))
test_list = json.loads(response.text)

#get dictionaries of time AND dict of duration
d_list = [item for item in test_list if item['dim'] == 'time']	
duration_list = [item for item in test_list if item['dim'] =='duration']

#modify value of duration_dictionary from 2 час -> PT2H
value_Duration= [sub['value']['value'] for sub in duration_list]
unit_Duration = [sub['value']['unit'] for sub in duration_list]

#modify unit_Duration
for i, word in enumerate(unit_Duration):
    if word == 'year':
        unit_Duration[i] = 'Y'
    if word == 'month':
        unit_Duration[i] = 'M'
    if word == 'week':
        unit_Duration[i] = 'W'
    if word == 'day':
        unit_Duration[i] = 'D'
    if word == 'hour':
        unit_Duration[i] = 'H'
    if word == 'minute':
        unit_Duration[i] = 'Min'
    if word == 'second':
        unit_Duration[i] = 'S'

list_valueD = []

for i in range(len(unit_Duration)):
    if unit_Duration[i] == 'Y' or unit_Duration[i] =='M' or unit_Duration[i]=='W' or unit_Duration[i] == 'D':
        valueD = "P" + str(value_Duration[i]) + unit_Duration[i] 
    if unit_Duration[i] == 'H' or unit_Duration[i] =='Min' or unit_Duration[i] == 'S':
        valueD = "PT" + str(value_Duration[i]) + unit_Duration[i] 
    list_valueD.append(valueD)
print(list_valueD) 
   

print(str(duration_list))
list_of_TE= d_list
print(str(list_of_TE))
#add items of key 'body' in duration_list to general list body 

body  = [ sub['body']for sub in list_of_TE ] 
body.extend([sub['body']for sub in duration_list])

#add items of key 'value' 
valueTimeX = [ sub['value']['value'] for sub in list_of_TE ] 
valueTimeX.extend(list_valueD)

#add items (value) of key 'dim' ('duration') in duration_list to general list typeTime
typeTime = [ sub['value']['grain'] for sub in list_of_TE ] 
typeTime.extend([sub['dim']for sub in duration_list])

#proccessing Type of TIMEML
for i, word in enumerate(typeTime):
    if word == 'day':
        typeTime[i] = 'DATE'
    if word == 'hour':
        typeTime[i] = 'TIME'
    if word == 'minute':
        typeTime[i] = 'TIME'
    if word == 'duration':
        typeTime[i] = 'DURATION'

print(str(body))
print(str(valueTimeX))
print(str(typeTime))

def __getTIMEX3Str(tid, timexType, value, timex):
        TIMEX3_TID = "<TIMEX3 tid=\"t"
        TIMEX3_TYPE = "\" type=\""
        TIMEX3_VALUE = "\" value=\""
        TIMEX3_MID = "\">"
        TIMEX3_END = "</TIMEX3>"
        return TIMEX3_TID + str(tid) + TIMEX3_TYPE + timexType + TIMEX3_VALUE + value + TIMEX3_MID + timex + TIMEX3_END

i=0
timexType = typeTime[0]
value = valueTimeX[0]
timex = body[0]
strAll= " "
for i in range(len(body)):
  tid = i
  timexType = typeTime[i]
  value = valueTimeX[i]
  timex = body[i]
  strAll = strAll + __getTIMEX3Str(tid, timexType, value, timex) + "\n"	

print(strAll)



