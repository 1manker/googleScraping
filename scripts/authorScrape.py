import sys
import time
import codecs
import subprocess as sub
import requests
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from webdriver_manager.chrome import ChromeDriverManager
from bs4 import BeautifulSoup
import os
import mysql.connector
import os
from selenium.common.exceptions import *
import urllib3
from urllib.request import urlopen


def setup(input_link):
    internet_loop()
    global url
    url = str(input_link)
    global options
    options = Options()
    options.headless = True
    global driver
    driver = webdriver.Chrome(ChromeDriverManager().install(), chrome_options=options)
    driver.implicitly_wait(30)
    driver.get(url)
    global link
    link = url.split("=")[1]
    print(url)
    print(link)
    execute_search()


def click_to_end():
    internet_loop()
    more_button = driver.find_element_by_id("gsc_bpf_more")
    while more_button:
        more_button = driver.find_element_by_id("gsc_bpf_more")
        if not more_button.is_enabled():
            break
        more_button.click()
        time.sleep(1)


def find_author():
    internet_loop()
    soup = BeautifulSoup(driver.page_source, 'lxml')
    author_bulk = soup.find('div', attrs={"id": "gsc_prf_in"})
    return author_bulk.getText()


def find_paper_title():
    internet_loop()
    soup = BeautifulSoup(driver.page_source, 'lxml')
    paper_title = soup.find('a', attrs={"class": "gsc_vcd_title_link"})
    if paper_title is None:
        return ''
    else:
        return paper_title.text


def iterate_through_pages():
    internet_loop()
    while len(driver.find_elements_by_xpath("//*[@id='gs_nm']/button[2]")) != 0:
        driver.find_element_by_xpath("//*[@id='gs_nm']/button[2]").click()
        time.sleep(2)


def iterate_through_links():
    internet_loop()
    index = 1
    try:
        while len(driver.find_elements_by_xpath("//*[@id='gsc_a_b']/tr[" + str(index) + "]/td[1]/a")) != 0:
            driver.find_element_by_xpath("//*[@id='gsc_a_b']/tr[" + str(index) + "]/td[1]/a").click()
            time.sleep(2)
            title = find_paper_title()
            time.sleep(2)
            data_group = pull_data()
            time.sleep(4)
            exp_to_db(author, title, data_group)
            driver.find_element_by_xpath("//*[@id='gs_md_cita-d-x']").click()
            time.sleep(2)
            index += 1
    except NoSuchWindowException:
        err_code(8)
        exit(8)
    finally:
        return


def exp_to_db(name, this_title, arr):
    db_loop()
    if len(this_title) < 1:
        return
    internet_loop()
    desc = pull_desc()
    connection = mysql.connector.connect(
        host="uwyobibliometrics.hopto.org",
        database="bibliometrics",
        user="luke",
        password="K8H,3Cuq]?HzG*W7",
        auth_plugin="mysql_native_password"
    )
    cursor = connection.cursor(prepared=True)
    for x in arr:
        sql_input = ("insert into citations (link, year, count, title, author, description, pub_date)" +
                     "values(%s, %s, %s, %s, %s, %s, %s) on duplicate key update count = %s")
        prep_inputs = (link, x.split(",")[0], x.split(",")[1], this_title, name, desc, x.split(",")[2],
                       x.split(",")[1])
        cursor.execute(sql_input, prep_inputs)
        connection.commit()
    connection.close()
    return


def pull_data():
    new_soup = BeautifulSoup(driver.page_source, 'lxml')
    dates = []
    cit_count = []
    data = []
    vals = []
    for x in new_soup.findAll('div', attrs={"class": "gs_scl"}):
        for y in x.findAll('div', attrs={"class": "gsc_vcd_value"}):
            vals.append(y.text)
    pub = vals[1]
    pub = pub.split("/")[0]
    for x in new_soup.findAll('div', attrs={"id": "gsc_vcd_graph_bars"}):
        for y in x.findAll('a', href=True):
            temp = (y['href']).split("yhi=")
            dates.append(temp[1])
    for x in new_soup.findAll('span', attrs={"class": "gsc_vcd_g_al"}):
        cit_count.append(x.text)
    for x in range(len(dates)):
        data.append(dates[x] + "," + cit_count[x] + "," + pub)
    return data


def pull_desc():
    new_soup = BeautifulSoup(driver.page_source, 'lxml')
    desc = new_soup.find('div', attrs={"class": "gsh_csp"})
    if desc is None:
        return ''
    return desc.getText()


def update_prof():
    db_loop()
    new_soup = BeautifulSoup(driver.page_source, 'lxml')
    inst = new_soup.findAll('div', attrs={"class": "gsc_prf_il"})
    if len(inst) < 1:
        return
    connection = mysql.connector.connect(
        host="uwyobibliometrics.hopto.org",
        database="bibliometrics",
        user="luke",
        password="K8H,3Cuq]?HzG*W7",
        auth_plugin="mysql_native_password"
    )
    cursor = connection.cursor(prepared=True)
    sql_input = "update profiles set author = %s, institution = %s where link = %s"
    prep_input = (author, inst[0].getText(), link)
    cursor.execute(sql_input, prep_input)
    connection.commit()
    connection.close()


def set_search_flag():
    db_loop()
    connection = mysql.connector.connect(
        host="uwyobibliometrics.hopto.org",
        database="bibliometrics",
        user="luke",
        password="K8H,3Cuq]?HzG*W7",
        auth_plugin="mysql_native_password"
    )
    cursor = connection.cursor(prepared=True)
    sql_input = (link,)
    add_s_flag = "update profiles set search_date = NOW() where link = %s"
    cursor.execute(add_s_flag, sql_input)
    connection.commit()
    connection.close()


def execute_search():
    err_code(1)
    click_to_end()
    time.sleep(1)
    global author
    author = find_author()
    update_prof()
    iterate_through_links()
    set_search_flag()
    try:
        driver.close()
    except NoSuchWindowException:
        err_code(8)
        exit(8)
    err_code(0)


def err_code(code):
    db_loop()
    connection = mysql.connector.connect(
        host="uwyobibliometrics.hopto.org",
        database="bibliometrics",
        user="luke",
        password="K8H,3Cuq]?HzG*W7",
        auth_plugin="mysql_native_password"
    )
    cursor = connection.cursor(prepared=True)
    sql_input = (link,)
    add_s_flag = "update profiles set err_flag =" + str(code) + " where link = %s"
    cursor.execute(add_s_flag, sql_input)
    connection.commit()
    connection.close()


def internet_on():
    try:
        response = urlopen('https://www.google.com/', timeout=10)
        return True
    except:
        return False


def db_on():
    try:
        db = mysql.connector.connect(
         host="uwyobibliometrics.hopto.org",
         database="bibliometrics",
         user="luke",
         password="K8H,3Cuq]?HzG*W7",
         auth_plugin="mysql_native_password"
        )
        cursor = db.cursor()
        cursor.execute("SELECT VERSION()")
        results = cursor.fetchone()
        if results:
            db.close()
            return True
        else:
            return False
    except mysql.connector.Error as err:
        print("ERROR IN CONNECTION")
    return False


def db_loop():
    if not db_on():
        print("connection to database lost, retrying in 10 minutes")
        time.sleep(600)
        db_loop()


def internet_loop():
    if not internet_on():
        print("network connection lost, retrying in 10 minutes")
        time.sleep(600)
        internet_loop()
