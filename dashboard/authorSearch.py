from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup

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
	soup = BeautifulSoup(driver.page_source, 'html')
	author_bulk = soup.find_all('h3', attrs={"class": "gs_ai_name"})
	description_bulk = soup.find_all('div', attrs={"class": "gs_ai_aff"})
	ret = []
	for x, y in zip(author_bulk, description_bulk):
		ret.append(x.getText() + "~~~" + y.getText())
	return ret

