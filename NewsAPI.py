import pandas as pd
import requests
import json
import os

people = ['\"Bernie Sanders\"',
          '\"Donald Trump\"',
          'Kamala',
          'Beto',
          'Biden',
          'Buttigieg',
          '\"Elizabeth Warren\"'
          ]

sources = ['the-new-york-times',
           'the-washington-post',
           'fox-news',
           'rt',
           'breitbart-news',
           'daily-mail'
           ]

data = pd.DataFrame()

# Loop through each person
for person in people:

    # Loop through all sources
    for source in sources:

        print(person + " " + source)

        url = "https://newsapi.org/v2/everything"

        querystring = {"apiKey": os.environ['news_sentiment_key'],
                       "language": "en",
                       "pageSize": "100",
                       "page": '1',
                       "sources": source,
                       "q": person
                       }

        payload = ""
        headers = {
            'cache-control': "no-cache",
            'Postman-Token': "425eca2d-d08f-4ce6-86e6-7a1f7d528100"
        }

        response = requests.request("GET", url, data=payload, headers=headers, params=querystring)

        # If there are no results move on to the next one
        if json.loads(response.text)['totalResults'] == 0:
            break

        temp_data = pd.io.json.json_normalize(json.loads(response.text)['articles'])

        # add column to temp data identifying who was searched for
        temp_data['subject'] = person

        for index, row in temp_data.iterrows():
            # Check each row to see if it can be encoded with ansi
            try:
                row.to_csv(os.environ['filepath'] + '/Tests/test.csv',
                           index=False, 
                           encoding='ansi')
          
            except UnicodeEncodeError:
                # if ansi encoding does not work, attempt to remove the problem characters
                print(row['content'])
                print(row['description'])
                print(row['title'])
                print()
                row['content'] = (row.str.replace(r'[^\x00-\x7F]+', ''))['content']
                row['description'] = (row.str.replace(r'[^\x00-\x7F]+', ''))['description']
                row['title'] = (row.str.replace(r'[^\x00-\x7F]+', ''))['title']

        data = data.append(temp_data, sort=False)

# Export data
data.to_csv(os.environ['filepath'] + '/rawdata.csv',
            index=False, 
            encoding='ansi')
