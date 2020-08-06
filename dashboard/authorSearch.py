from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import time

def search(author):
	global url
	url = "https://scholar.google.com/citations?view_op=search_authors&hl=en&mauthors=" + author.replace(' ', '+')
	print(url)
	global options
	options = webdriver.ChromeOptions()
	options.add_argument('headless')
	global driver
	DRIVER_PATH = 'C:/ProgramData/chocolatey/lib/chromedriver/tools/chromedriver'
	driver = webdriver.Chrome(executable_path=DRIVER_PATH, chrome_options=options)
	driver.implicitly_wait(30)
	driver.get(url)
	ret = []
	for x in range(1,5):
		soup = BeautifulSoup(driver.page_source, 'html')
		author_bulk = soup.find_all('h3', attrs={"class": "gs_ai_name"})
		description_bulk = soup.find_all('div', attrs={"class": "gs_ai_aff"})
		for x, y in zip(author_bulk, description_bulk):
			link = x.find('a')['href'].split("=")
			ret.append([x.getText(),y.getText(),link[2]])
		if len(driver.find_elements_by_xpath("/html/body/div/div[8]/div[2]/div/div[11]/div/button[2]")) != 0:
			driver.find_element_by_xpath("/html/body/div/div[8]/div[2]/div/div[11]/div/button[2]").click()
			time.sleep(1)
		else:
			break    

	return ret
