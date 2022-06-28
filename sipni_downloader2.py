#!/usr/bin/python3
import os, sys
from datetime import date, timedelta, datetime
from lxml import html 
import requests 
import re
import locale
locale.setlocale(locale.LC_TIME, "pt_BR.UTF-8")

def get_file(download_address, output_file):
    r = requests.get(download_address, verify=False, allow_redirects=True,
                     stream = True, timeout=100)
    print(f"=== download size: {round(int(r.headers.get('content-length')) / (1024*1024))} M ===\n")
    with open(output_file, 'wb') as f:
        # 100M chunk size
        chunk_size = 100 * 1024 * 1024
        for chunk in r.iter_content(chunk_size=chunk_size):
            f.write(chunk)

def get_UF_file(index_page_address, UF, output_file):
    page = requests.get(index_page_address, verify=False, timeout=10)
    tree = html.fromstring(page.content)
    #e = tree.xpath(f'.//a[text()="Dados {UF}"]')
    regexpNS = "http://exslt.org/regular-expressions"
    e = tree.xpath(f'//a[re:test(., "Dados {UF}( Parte [0-9])*")]', namespaces={"re": regexpNS})

    if len(e) == 0:
        return False
    if len(e) == 1:
        get_file(e[0].attrib['href'], output_file)
    else:
        for i, ei in enumerate(e):
            get_file(ei.attrib['href'], output_file + '.' + str(i))
        os.system(f'mv {output_file}.0 {output_file}')
        for i in range(1, len(e)):
            os.system(f'''tail -n +2 {output_file}.{i} >> {output_file} && 
                          rm {output_file}.{i}''')
    return True

def get_date(index_page_address):
    page = requests.get(index_page_address, verify=False, timeout=10)
    tree = html.fromstring(page.content)
    data = tree.xpath('.//th[text()="Dados atualizados pela última vez"]')[0].getparent().getchildren()[1].text
    return datetime.strptime(data, "%d/%B/%Y")

def check_for_new_file(index_page_address, last_date):
    page = requests.get(index_page_address, verify=False, timeout=10)
    tree = html.fromstring(page.content)
    resources = tree.xpath('//li[@class="resource-item"]')
    reg = re.compile(r".*SRAG (\d\d/\d\d/\d\d\d\d).*",
            re.DOTALL|re.MULTILINE|re.IGNORECASE)
    for item in resources:
        g = reg.match(item.text_content())
        if g:
            data_read = datetime.strptime(g.groups()[0], "%d/%m/%Y").date()
            if data_read > last_date:
                address = item.xpath('.//a[@class="resource-url-analytics"]')[0].attrib['href']
                return (data_read, address)
    return False

if __name__ == '__main__':
    index_page_address1 = "https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/5093679f-12c3-4d6b-b7bd-07694de54173"
    index_page_address2 = "https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/10aed154-04c8-4cf4-b78a-8f0fa1bc5af4"
    estados1 = ["AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
            "MG", "MS", "MT"]
    estados2 = [ "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO",
            "RR", "RS", "SC", "SE", "SP", "TO"]
    output_folder = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'dados/')

    data1 = get_date(index_page_address1)
    data2 = get_date(index_page_address1)
    if data1 != data2:
        print("Datas de atualização discordam! Saindo....")
        sys.exit(1)
    print(f'data da última atualização: {data1.strftime("%Y-%m-%d")}')
    if len(sys.argv) == 1:
        print("USO: sipni_downloader [UF1] [UF2] ... | [todas] -dYYYY-mm-dd")
        sys.exit(0)

    UFs = []
    data_ant = False
    for arg in sys.argv[1:]:
        if arg[:2] == "-d":
            data_ant = datetime.strptime(arg[2:], "%Y-%m-%d")
        else:
            UFs.append(arg)
    if 'todas' in UFs:
        UFs = estados1 + estados2
    if data_ant and data1 <= data_ant:
        print("Base não foi atualizada desde a data pedida.")
        sys.exit(2)
    for UF in UFs:
        if UF in estados1:
            print(f'=== baixando base de {UF} ===\n')
            fname = f'dados_{data1.strftime("%Y-%m-%d")}_{UF}.csv'
            output_file = os.path.join(output_folder, fname)
            get_UF_file(index_page_address1, UF, output_file)
        elif UF in estados2:
            print(f'=== baixando base de {UF} ===\n')
            fname = f'dados_{data2.strftime("%Y-%m-%d")}_{UF}.csv'
            output_file = os.path.join(output_folder, fname)
            get_UF_file(index_page_address2, UF, output_file)
        else:
            print(f'\n   "{UF}" não é uma UF válida\n')

