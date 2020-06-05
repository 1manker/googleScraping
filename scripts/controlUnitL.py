import sys
import time
import codecs
import subprocess as sub
import requests
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import mysql.connector
import os
import subprocess
import authorScrape

finished = False


def check_queue():
        authorScrape.db_loop()
        authorScrape.internet_loop()
        connection = mysql.connector.connect(
         host="uwyobibliometrics.hopto.org",
         database="bibliometrics",
         user="luke",
         password="K8H,3Cuq]?HzG*W7",
         auth_plugin="mysql_native_password"
        )
        sql_query = "select link from profiles where queue_status = false and search_date IS NULL"
        cursor = connection.cursor(prepared=True)
        cursor.execute(sql_query)
        results = cursor.fetchall()
        if len(results) < 1:
            global finished
            finished = True
            return
        time.sleep(5)
        add_q_flag = "update profiles set queue_status = true where link = %s"
        link = results[0][0].decode("utf-8")
        sql_input = (link,)
        cursor.execute(add_q_flag, sql_input)
        connection.commit()
        connection.close()
        code = "https://scholar.google.com/citations?user=" + link
        authorScrape.setup(code)


while not finished:
    check_queue()


