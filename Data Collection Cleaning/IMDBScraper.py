#!/usr/bin/env python
"""Scrapes www.imdb.com for movie information and star meter ratings"""

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import urllib2
from bs4 import BeautifulSoup

def getWebPage(url):
	soup = None
	while not soup:
		try:
			print ("Connecting..")
			browse = urllib2.urlopen(url)
			soup = BeautifulSoup(browse.read())
		except:
			print ("Connection Lost. Internet Fucking up")
		
	
	return soup

#Check if the movie is within the start and end
def getLinks(movies, boxes, start, end):
	print (movies[0].find("a").text, len(movies), len(boxes))
	year = [yr.find("span",class_="year_type").text.strip("()") for yr in movies ]
	box = [bx.text for bx in boxes]
	movie = [mv.find("a").text.strip() for mv in movies]
	link = [lk.find("a")["href"] for lk in movies]
	print ("Length of different lists:",len(year),len(box),len(movie),len(link))
	rtnList = []
	for yrindex in range(len(year)):
		if year[yrindex].isdigit() and int(year[yrindex]) in range(start, end+1):
			if len(box) == len(year) and box[yrindex].startswith("$"):
				rtnList.append([movie[yrindex],link[yrindex],year[yrindex]])
			elif len(box) != len(year):
				rtnList.append([movie[yrindex],link[yrindex],year[yrindex]])
	print ("The length of the list returned is : ",len(rtnList))
	return rtnList

#Get Links of all movies to be scraped
def linkScraper(base, start = 2013, end = 2014):
	#Initialise the variable to be returned
	linkInfo = {}
	movieName = []
	movieLink = []
	movieYear = []
	#Open browser Instance to scrape the links
	browser = urllib2.urlopen(base)
	soup = BeautifulSoup(browser.read())
	nextObj = soup.find("span",class_ = "pagination").find("a")
	if nextObj.text.find("Next") == -1:
		nextObj = None
	movies = soup.find_all("td",class_ = "title")
	boxes = soup.find_all("td",class_ = "sort_col")
	rtnList = getLinks(movies,boxes,start,end)
	for index in range(len(rtnList)):
		movieName.append(rtnList[index][0])
		movieLink.append(rtnList[index][1])
		movieYear.append(rtnList[index][2])
	open("DataObjects/link.txt","w").write(nextObj["href"] + "\n")
	#Loop to go through other pages:
	while nextObj:
		soupObj = None
		open("DataObjects/link.txt","a").write(nextObj["href"] + "\n")
		while not soupObj:
			try:
				soupObj = BeautifulSoup(urllib2.urlopen("http://www.imdb.com"+nextObj["href"]).read())
			except:
				print("Check Network Connection")
		try:
			nextObj = soupObj.find("span",class_ = "pagination").find_all("a")
		except:
			print ("Exception occured")
			linkInfo = {'Name':movieName,'Link':movieLink,'Year':movieYear}
			return linkInfo	

		if len(nextObj) == 1 and nextObj[0].text.find("Next") == -1:
			nextObj = None
		elif len(nextObj) == 2 and nextObj[1].text.find("Next") == -1:
			nextObj = None
		elif len(nextObj) == 1:
			nextObj = nextObj[0]
		elif len(nextObj) == 2:
			nextObj = nextObj[1]

		try:
			movies = soupObj.find_all("td",class_ = "title")
			boxes = soupObj.find_all("td",class_ = "sort_col")
			
		except:
			print ("Exception occured 2nd time")
			linkInfo = {'Name':movieName,'Link':movieLink,'Year':movieYear}
			return linkInfo		
		rtnList = getLinks(movies,boxes,start,end)
		
		for index in range(len(rtnList)):
			movieName.append(rtnList[index][0])
			movieLink.append(rtnList[index][1])
			movieYear.append(rtnList[index][2])
	linkInfo = {'Name':movieName,'Link':movieLink,'Year':movieYear}
	return linkInfo

#Function to pull a movie's data: Actors, director, Budget, production house
def pullMovieDetail(movieSoup):
	#Get the required Movie Name - Rating
	movieName = movieSoup.find("h1",class_ = "header").find("span").text.encode('utf-8')
	try:
		movieRating = float(movieSoup.find("div", class_ = "titlePageSprite star-box-giga-star").text)
	except:
		print "Rating not present. Shitty movie"
		return []
	try:
		movieDir = movieSoup.find("div",itemprop = "director").find("a").text.encode('utf-8')
	except:
		print("Director Missing. Lousy Movie")
		return []
	try:
		movieWriter = movieSoup.find("div",itemprop = "creator").find("a").text.encode('utf-8')
	except:
		print ("Writer info not present. Shitty movie again")
		return []
	try:
		cast_table = movieSoup.find("table",class_ = "cast_list").find_all("td",itemprop = "actor")
	except:
		print ("Movie without Actors?! Skip!!")
		return []
	movieActor = [actor.text.encode('utf-8').strip() for actor in cast_table]
	details = movieSoup.find("div",id = "titleDetails").find_all("div",class_ = "txt-block")
	movieBudget = ""
	movieProd = []
	for i in details:
		if i.findChild().text.find("Budget") != -1:
			tempText = i.text
			tempNum = [s for s in tempText if s.isdigit()]
			for i in tempNum:
				movieBudget+= i
			movieBudget = int(movieBudget)
		elif i.findChild().text.find("Production") != -1:
			prod = [pr.text.encode('utf-8').strip() for pr in i.find_all("a")]
			movieProd = prod[:2]
	if movieProd == []:
		print("Production company not there. Free movie")
		return []
	
	movieDetail = [movieName, movieRating, movieDir, movieWriter, movieActor, movieBudget, movieProd]
	
	return movieDetail
			
	
def movieScraper(Links):
	#Links is a list object which contains the movie links scraped for the specific time period.
	#This is being passed in order to scrape the movie details in those links.
	#Important to note that scraping is done only on hollywood movies as they present best environment to model
	#The first condition being carried out is to check if the given movie is a hollywood movie.
	#Return a structured dataset containing movie name, Rating, Budget, Cast(Specific number), Director, Production Co(first 2), Genre (All)
	movieDetail = []
	
	for link in Links:
		print link,"    Movie #:",Links.index(link), " of ",len(Links), "Data: ",len(movieDetail)
		soup = getWebPage(link)
		#Find the country that the movie is associated with and check if its from USA
		try:
			divTxt = soup.find("div",id = "titleDetails").find_all("div",class_ = "txt-block")
		except:
			print "Exception Occured. Continuing"
			continue
		country = []
		for i in divTxt:
			
			if i.findChild().text.find( "Country") != -1:
				country = i.find_all("a")
		checkHollywood = 0
		for i in country:
			if i.text == "USA":
				print("A hollywood movie!")
				checkHollywood = 1

		if checkHollywood == 0:
			print ("Not a Hollywood movie. Good riddance")
			continue
		
		result = pullMovieDetail(soup)
		if result != []:
			print("Getting proper result", len(result))
			movieDetail.append(result)
	
	return movieDetail


def actorScraper(base):
	#Get the actor starmeter rating and whether its an actor or actress
	actor = {}
	actorName = []
	actorRank = []
	actorGender = []
	url = base
	soup = BeautifulSoup(urllib2.urlopen(base).read())
	nextObj = "dummyinit"
	
	while len(actorName)<= 50000:
		print ("Number of Actors got:	",len(actorName))
		
		while not soup:
			try:
				print("Connecting")
				soup = BeautifulSoup(urllib2.urlopen("http://www.imdb.com"+nextObj["href"]).read())
				#open("DataObjects/ActorLinks.txt","w").write("http://www.imdb.com"+nextObj["href"])
			except:
				print("Check Network Connection")
		nextObj = None
		try:
			print ("Entering try block")
			nextObj = soup.find("span",class_ = "pagination").find_all("a")
		except:
			print ("Exception occured")
			actor = {"Name":actorName, "Rank":actorRank}
			return actor
				

		if len(nextObj) == 1 and nextObj[0].text.find("Next") == -1:
			nextObj = None
		elif len(nextObj) == 2 and nextObj[1].text.find("Next") == -1:
			nextObj = None
		elif len(nextObj) == 1:
			nextObj = nextObj[0]
		elif len(nextObj) == 2:
			nextObj = nextObj[1]

	
		table = soup.find("table",class_ = "results")
		tdName = table.find_all("td",class_ = "name")
		tdRank = table.find_all("td",class_ = "number")

		if len(tdName) == len(tdRank):
			for i in range(len(tdName)):
				actorName.append(tdName[i].find("a").text.encode("utf-8").strip())
				#print(tdName[i].find("a").text.encode("utf-8").strip())
				actorRank.append(int(tdRank[i].text.encode("utf-8").strip(".")))

		else:
			print("The lengths of the two lists are unequal. Not putting into the final array.")
		soup = None
		if nextObj == None:
			break

	actor = {"Name":actorName, "Rank":actorRank}
	return actor

	
		
	 
