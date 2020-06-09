from IPython.display import display
from natasha import (
    Segmenter,
    MorphVocab,
    
    NewsEmbedding,
    NewsMorphTagger,
    NewsSyntaxParser,
    NewsNERTagger,
    
    PER,
    NamesExtractor,

    Doc
)
import requests
import json

segmenter = Segmenter()
morph_vocab = MorphVocab()

emb = NewsEmbedding()
morph_tagger = NewsMorphTagger(emb)
syntax_parser = NewsSyntaxParser(emb)
ner_tagger = NewsNERTagger(emb)

names_extractor = NamesExtractor(morph_vocab)

file = open("corpus.txt",'rb')
text = file.read()
doc = Doc(text.decode("utf-8"))

#text='фильм идет 2 часа. Вчера я пошел в школу'
#doc = Doc(text)
doc.segment(segmenter)
doc.tag_morph(morph_tagger)
for token in doc.tokens:
     token.lemmatize(morph_vocab)
list_tokens=doc.tokens[:]


display(doc.tokens[2])
display(doc.sents[:])
#token_list = [item['Doctoken']['text']  for item in list_tokens]	

Data = {'locale':'ru_RU','text':text}
url='http://0.0.0.0:8000/parse'
response = requests.post(url,data=Data)
text=response.text
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
value_TimeX = [ sub['value']['value'] for sub in list_of_TE ] 
value_TimeX.extend(list_valueD)
valueTimeX = []
for string in value_TimeX:
	new_string= string.replace('.000-07:00','')
	valueTimeX.append(new_string)
	
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
print(valueTimeX)
print(str(typeTime))
#Function make TIMEX3str
def __getTIMEX3Str(tid, timexType, value, timex):
        TIMEX3_TID = "<TIMEX3 tid=\"t"
        TIMEX3_TYPE = "\" type=\""
        TIMEX3_VALUE = "\" value=\""
        TIMEX3_MID = "\">"
        TIMEX3_END = "</TIMEX3>"
        return TIMEX3_TID + str(tid) + TIMEX3_TYPE + timexType + TIMEX3_VALUE + value + TIMEX3_MID + timex + TIMEX3_END
#Funtion make TIMEX3 dictionary 
def __createDictTimeX3(tid, timexType, value, timex):
	dict_Timex3 ={}	
	dict_Timex3['TIMEX3_TID'] = str(tid)
	dict_Timex3['TIMEX3_TYPE'] = timexType
	dict_Timex3['TIMEX3_Value'] = value
	dict_Timex3['TIMEX3_BT'] = timex
	return dict_Timex3

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

#Represent anno TIMEX3 with dictionaries
List_Of_Dict_Timex3 = []
for i in range(len(body)):
  tid = i
  timexType = typeTime[i]
  value = valueTimeX[i]
  timex = body[i]
  dictionary_timex3 = __createDictTimeX3(tid, timexType, value, timex)
  List_Of_Dict_Timex3.append(dictionary_timex3)

print(List_Of_Dict_Timex3)	


#function tokenize and lemma

def __FuncTokLem(text):
 doc = Doc(text)

 doc.segment(segmenter)
 doc.tag_morph(morph_tagger)
 for token in doc.tokens:
     token.lemmatize(morph_vocab)
 return doc.tokens[0].text

#Extraction DURATION or TIME
for i in range(0,len(body)):
 for j in range(i+1,len(typeTime)):
   if (body[i] == body[j]) and (timexType[i] != typeTime[j]):
      compare_word = __FuncTokLem(body[i])
      for k in range(len(list_tokens)):
         if ((doc.tokens[k].text == compare_word) & (doc.tokens[k-1].lemma == 'идти')) : 
          res = [items for items in List_Of_Dict_Timex3 if not (items['TIMEX3_TYPE'] == 'TIME' and items['TIMEX3_BT'] == body[i])] 
          print(res)
   

