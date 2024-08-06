from requests import get
from bs4 import BeautifulSoup
from tabulate import tabulate
from datetime import date, timedelta
import os
import plotext as plt
import json

#Limpiar consola (solo estético)
def clear():
    if os.name == 'nt':
      os.system("cls")
    else:
      os.system("clear")

#Diccionario Ligas
dic = {'super lig':'sueper-lig-8', 'liga portugal':'liga-portugal-35', 'eredivise':'eredivisie-36', 
         'ligue 1':'ligue-1-23', 'bundesliga':'bundesliga-1', 'serie a':'serie-a-13', 'la liga':'laliga-10',
           'premier league':'premier-league-9'}

def get_table_data(league):
    url = f'https://onefootball.com/en/competition/{dic[league]}/table'
    classificacion_raw = get(url)
    soup = BeautifulSoup(classificacion_raw.content, 'html.parser')
    rows = soup.find_all('li',{'class', "Standing_standings__row__5sdZG Standing_standings__rowLink__Skr86"})
    datos = {}
    for i, row in enumerate(rows):
        name = row.find('div', {'class':"Standing_standings__team__Lzl6d"}).text
        otro = row.find_all('div', {'class':"Standing_standings__cell__5Kd0W Standing_standings__cellTextDimmed__vpZYH"})
        cosa= row.find_all('div', {'class':"Standing_standings__cell__5Kd0W Standing_standings__cellLargeScreen__ttPap Standing_standings__cellTextDimmed__vpZYH"})
        Puntos = row.find('span',{'class':"title-7-bold"}).text 
        datos[name] = {'pos': i+1, 'PJ': otro[0].text, 'G': cosa[0].text,'E':cosa[1].text, 'D': cosa[2].text, 'DG': otro[1].text, 'PTS': Puntos}  
    return datos

def get_table(league):
    data = get_table_data(league)
    data2 = []
    for nom, data in data.items():
        l = [nom]
        cosa = list(data.values())
        for e in cosa[1:]:
            l.append(e)
        data2.append(l)
    data_columns = ['Equipo', 'PJ', 'PG', 'PE', 'PP', 'DG', 'Puntos' ]
    return tabulate(data2, headers=data_columns, tablefmt="pipe", showindex=[i for i in range(1, len(data2)+1)])

def top_partidos(league):
    url = f'https://onefootball.com/en/competition/{dic[league]}/fixtures'
    classificacion_raw = get(url)
    soup = BeautifulSoup(classificacion_raw.content, 'html.parser')
    data = soup.find_all('ul', {'class', 'grid MatchCardsList_matches__8_UwB'})
    data = data[0]
    team_data = get_table_data(league)
    fixtures = data.find_all('li')
    f_buenos = []
    for fixture in fixtures:
        link = fixture.find('a')['href']
        p = fixture.find_all('span', {'class', "SimpleMatchCardTeam_simpleMatchCardTeam__name__7Ud8D"})
        if (team_data[p[0].text]['pos'] + team_data[p[1].text]['pos'])/2 < 6:
            a = p[0].text + ' - ' + p[1].text
            f_buenos.append((a, link))

    return f_buenos

#Imprime el resultado de las dos funciones siguientes
def get_info(partido):
    match, link = partido
    stadium, date, time = match_info(link) 
    print(f'Estadio: {stadium}')
    print(f'Día y hora: {date}, {time}')
    print('ÚLITMOS 5 RESULTADOS')
    print(last_five_results(link))
    print()

#Devuelve estadio y fecha del partido
def match_info(link):
  url = f'https://onefootball.com/{link}'
  classificacion_raw = get(url)
  soup = BeautifulSoup(classificacion_raw.content, 'html.parser')
  #Info estadio
  table = soup.find_all('span', {'class':'title-8-regular MatchInfoEntry_subtitle__Mb7Jd'})
  stadium = table[2].text.strip()
  #Info fecha
  match_date = table[1].text
  if match_date.lower() == 'today':
      match_date = date.today().strftime("%d/%m/%Y")
  elif match_date.lower() == 'tomorrow':
      match_date = date.today() + timedelta(days=1)
      match_date = match_date.strftime("%d/%m/%Y")
  elif match_date.lower() == 'yesterday':
      match_date = date.today() + timedelta(days=-1)
      match_date = match_date.strftime("%d/%m/%Y")
  #Info hora
  try:
    hour = soup.find('span', {'class':'title-6-bold MatchScore_numeric__ke8YT'}).text
  except: 
    hour = '--:--'
  if hour == '--:--':
    print('EL PARTIDO YA SE HA JUGADO')

  return stadium, match_date, hour
  
  
#Tabula los últimos 5 resultados de ambos equipos
def last_five_results(link):
  url = f'https://onefootball.com/{link}'
  classificacion_raw = get(url)
  soup = BeautifulSoup(classificacion_raw.content, 'html.parser')

  results = []
  home_team = []
  away_team = []
  for section in soup.find_all('section', {'class':'FormGuide_container__mrljl'}):
      for li in section.find_all('li'):
        temp = []
        for result in li.find_all('p')[1:]:
          temp.append(result.text)
        results.append(' - '.join(temp))
        temp = []
        cont = 0
        for photo in li.find_all('img'):
          if cont % 2 == 0:
            home_team.append((photo['alt']))
            cont +=1
          else: 
            away_team.append((photo['alt']))
            cont += 1
      home_team.append(' ')
      results.append(' ')
      away_team.append(' ')
  home_team.pop()
  results.pop()
  away_team.pop()
  data2 = {'home_team':home_team, 'results':results, 'away_team' : away_team}
  data_columns = ['LOCAL', 'RESULTADO', 'VISITANTE']
  return tabulate(data2, headers=data_columns, tablefmt="pipe", stralign='center')

def get_squad_value(team): #devuelve el valor de plantilla con el api de tranfermarkt
    link = f'https://transfermarkt-api.vercel.app/clubs/search/{team}'
    page_data = get(link)
    data = json.loads(page_data.text)
    return data['results'][0]['marketValue']

def graph(matchup): #Se da el partido de equipo - equipo
    equipos = matchup.split(' - ')
    equipo1 =equipos[0]
    equipo2 =equipos[1]
    valor1 = get_squad_value(equipo1)
    valor2 = get_squad_value(equipo2)
    plt.simple_bar([equipo1,equipo2], [to_number(valor1),to_number(valor2)], title = 'Valor de Plantilla')
    plt.show()

def to_number(cosa): #convierte el valor (389m por ejemplo) a un numero (389000000) para hacer una grafica si queremos
    cosa = cosa.replace(cosa[0], '',1)
    if cosa[-1] == 'm':
        cosa = cosa.replace(cosa[-1],'',1)
        return int(float(cosa)*1000000)
    else:
        cosa = cosa.replace(cosa[-1],'',1)
        cosa = cosa.replace(cosa[-1],'',1)
        return int(float(cosa)*1000000000)




  



