import sys
import time
import codecs
import subprocess as sub
import requests
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import mysql.connector
import re

url = str(sys.argv[1])
options = Options()
options.headless = True
driver = webdriver.Chrome(ChromeDriverManager().install(), chrome_options=options)
driver.implicitly_wait(30)
driver.get(url)


def click_to_end():
    more_button = driver.find_element_by_id("gsc_bpf_more")
    while more_button:
        more_button = driver.find_element_by_id("gsc_bpf_more")
        if not more_button.is_enabled():
            break
        more_button.click()
        time.sleep(4)


def click_on_paper():
    count = 1
    time.sleep(2)
    click_to_end()
    time.sleep(3)
    while driver.find_element_by_xpath("//*[@id='gsc_a_b']/tr[" + str(count) + "]/td[2]/a").is_displayed():
        paper_button = driver.find_element_by_xpath("//*[@id='gsc_a_b']/tr[" + str(count) + "]/td[2]/a")
        time.sleep(1)
        paper_button.click()
        count += 1
        time.sleep(1)
        click_through_papers()
    driver.implicitly_wait(10)
    driver.get(url)


def click_through_papers():
    next_button = driver.find_element_by_xpath("//*[@id='gs_nm']/button[2]/span/span[1]")
    while next_button:
        next_button = driver.find_element_by_xpath("//*[@id='gs_nm']/button[2]/span/span[1]")
        if not next_button.is_enabled():
            return
        pull_profiles()
        time.sleep(300)
        next_button.click()
        time.sleep(10)


def pull_profiles():
    connection = mysql.connector.connect(
        host="uwyobibliometrics.hopto.org",
        database="bibliometrics",
        user="luke",
        password="K8H,3Cuq]?HzG*W7",
        auth_plugin="mysql_native_password"
    )
    soup = BeautifulSoup(driver.page_source, 'lxml')
    for x in soup.findAll('div', attrs={"class": "gs_a"}):
        for xs in x.findAll('a'):
            raw_link = str(xs.get('href'))
            web_link = raw_link.split("=")[1]
            web_link = web_link.split("&")[0]
            cursor = connection.cursor(prepared=True)
            sql_query = "select * from profiles where link = %s"
            sql_input = (web_link,)
            cursor.execute(sql_query, sql_input)
            searched = cursor.fetchall()
            if len(searched) < 1:
                sql_insert = "insert into profiles (link, queue_status) values(%s, false)"
                cursor.execute(sql_insert, sql_input)
                print("ok, inserted!")
                connection.commit()
    connection.close()


click_on_paper()
driver.close()

