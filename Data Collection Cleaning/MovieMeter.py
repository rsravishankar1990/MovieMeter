#!/usr/bin/env python

import IMDBScraper as isc
import pickle

base = "http://www.imdb.com/search/title?at=0&count=100&sort=boxoffice_gross_us&title_type=feature"
#link = isc.linkScraper(base)

#pickle.dump(link,open("DataObjects/MovieLinks.pkl","wb"))



#print len(link['Name'])
'''
link = ["http://www.imdb.com/title/tt0162222/","http://www.imdb.com/title/tt1891770/","http://www.imdb.com/title/tt0299854/"]
lst = pickle.load(open("DataObjects/MovieLinks.pkl","rb"))
lst = lst["Link"]
lst1 = lst[:2001]
lst2 = lst[2001:4001]
lst3 = lst[4001:6001]
lst4 = lst[6001:8001]
lst5 = lst[8001:]
lst1 = ["http://www.imdb.com"+i for i in lst1]
lst2 = ["http://www.imdb.com"+i for i in lst2]
lst3 = ["http://www.imdb.com"+i for i in lst3]
lst4 = ["http://www.imdb.com"+i for i in lst4]
lst5 = ["http://www.imdb.com"+i for i in lst5]
'''

#detail1 = isc.movieScraper(lst1)
#pickle.dump(detail1,open("DataObjects/MovieDetails1.pkl","wb"))
#detail2 = isc.movieScraper(lst2)
#pickle.dump(detail2,open("DataObjects/MovieDetails2.pkl","wb"))
#detail3 = isc.movieScraper(lst3)
#pickle.dump(detail3,open("DataObjects/MovieDetails3.pkl","wb"))
#detail4 = isc.movieScraper(lst4)
#pickle.dump(detail4,open("DataObjects/MovieDetails4.pkl","wb"))
#detail5 = isc.movieScraper(lst5)
#pickle.dump(detail5,open("DataObjects/MovieDetails5.pkl","wb"))


#print(len(detail5))

"""
actorBase = "http://www.imdb.com/search/name?gender=male,female&ref_=nv_tp_cel_1&start=1000001"
actor = isc.actorScraper(actorBase)

pickle.dump(actor,open("DataObjects/ActorRanks3.pkl","wb"))
"""


#Get the links of all movies from the IMDB database - ones on the box office search section
base = "http://www.imdb.com/search/title?at=0&count=100&sort=boxoffice_gross_us&title_type=feature"
link = isc.linkScraper(base)






