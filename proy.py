import funciones as f

leagues = ['Premier League', 'La Liga', 'Serie A', 'Bundesliga',  'Ligue 1', 'Eredivise', 'Liga Portugal', 'Super Lig']

while True:
    league = input(f'¿Qué liga quieres consultar? \n {leagues[0]:15} | {leagues[1]:15} | {leagues[2]:15} | {leagues[3]:15} \n {leagues[4]:15} | {leagues[5]:15} | {leagues[6]:15} | {leagues[7]:15} \n >')

    league = league.lower().strip()

    fixture = input('¿Quieres ver la clasificación o los mejores partidos? \n Escribe \"C\" (Clasificación) o \"P\" (Partidos) \n >')

    if fixture.lower().strip() == 'c': 
        f.clear()
        print(f.get_table(league))
    elif fixture.lower().strip() == 'p':
        partidos = f.top_partidos(league)
        if len(partidos) == 0:
            print('No hay partidos competitivos esta jornada.')
        else:
            f.clear()
            print('Partidos más competitivos de esta jornada:')
            for i, partido in enumerate(partidos):
                print(f'{i+1}. {partido[0]}')
            print("\n")
            while True:
                interesante = input('Cual te interesa? ') if len(partidos) > 1 else '1'
                if interesante.isdigit() and int(interesante) <= len(partidos):
                    break
                else: print('Escribe un número válido. ', end = '')
            print("\n")
            f.get_info(partidos[int(interesante)-1])
            print("\n")
            print(f.graph(partidos[int(interesante)-1][0]))
    elif fixture.lower() == 'no':
      exit()
    print("\n"*2)


    


    
    