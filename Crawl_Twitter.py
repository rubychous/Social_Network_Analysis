# import library
import tweepy

# set up basic infomation
consumer_key='my key'
consumer_secret='my secret'
access_token='my token'
access_token_secret='my token secret '
auth= tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.secure= True
auth.set_access_token(access_token, access_token_secret)
api_json= tweepy.API(auth, parser=tweepy.parsers.JSONParser())
api= tweepy.API(auth)
user=api_json.get_user(userid)

#profile
User_Profile=api_json.me()
print("Showing @%(screen_name)s's profile."%{"screen_name":user["screen_name"]})
print("created_at: %(created_at)s"%User_Profile)
print("Screen Name: %(screen_name)s"%User_Profile)
print("User Name: %(name)s"%User_Profile)
print("User ID: %(id)d"%User_Profile)
print("User Location: %(location)s"%User_Profile)
print("User Description: %(description)s"%User_Profile)
print("The Number of Followers: %(followers_count)d"%User_Profile)
print("The Number of Friends: %(friends_count)d"%User_Profile)
print("The Number of Statuses: %(statuses_count)d"%User_Profile)
print("The URL of Profile Image: %(profile_image_url)s"%User_Profile)
print("User URL: %(url)s"%User_Profile)

#Followers 
print("Showing @%(screen_name)s's followers list. "%{"screen_name":user["screen_name"]})
for followers in api.followers():
    print("Screen Name:",followers.screen_name)

#Followees
print("Showing @%(screen_name)s's followees list."%{"screen_name":user["screen_name"]})
for friends in api.friends_ids():
    name=api.get_user(friends).screen_name
    print("Screen Name:",name)

#Home_timeline
Home_Timeline=api_json.home_timeline() 
print("Showing @%(screen_name)s's home timeline."%{"screen_name":user["screen_name"]})
for status in Home_Timeline:
    print('@%s : %s' % (status['user']['screen_name'], status['text']))
