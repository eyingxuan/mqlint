from random import random
from pymongo import MongoClient
from faker import Faker
import sys
import json
import pprint

fake = Faker()



def makeColl1Object(id):
  intId = random() < .5
  return {
    "id": id,
    "firstName": fake.first_name(),
    "lastName": fake.last_name(),
    "pets": [ {"id": int(random() * 1000) if intId else fake.name()} for _ in range(int(random() * 5))] 
  }

def makeColl2Objectv1(id):
  return {
    "id": id,
    "address": {
      "version": "v1",
      "longAddr": fake.address()
    }
  }

def makeColl2Objectv2(id):
  return {
    "id": id,
    "address": {
      "version": "v2",
      "addr": fake.street_address(),
      "zip": int(fake.zipcode())
    }
  }

def makeColl3Objectv1(id):
  return {
    "version": "v1",
    "id": id,
    "bankAccount": fake.iban()
  }

def makeColl3Objectv2(id):
  return {
    "version": "v2",
    "id": id, 
    "bankName": fake.company(),
    "bankId": fake.bban()
  }

def makeEntities():
  ssn = fake.ssn()
  v2 = random() < .5
  v3 = random() < .5
  return makeColl1Object(ssn), makeColl2Objectv2(ssn) if v2 else makeColl2Objectv1(ssn), makeColl3Objectv1(ssn) if v3 else makeColl3Objectv2(ssn)

def fillDB(coll1, coll2, coll3):
  coll1.delete_many({})
  coll2.delete_many({})
  coll3.delete_many({})
  for _ in range(100):
    doc1, doc2, doc3 = makeEntities()
    coll1.insert_one(doc1)
    coll2.insert_one(doc2)
    coll3.insert_one(doc3)

def runAggregation(filename, coll, client):
  pipeline = json.load(open(filename))
  result = client.test[coll].aggregate(pipeline=pipeline)
  print(list(result))
  # pprint.pprint(list(result))

if __name__ == '__main__':
  if len(sys.argv) < 2:
    print("""
    usage:
    python demo.py fill
      - fill collections with fake data
    python demo.py run <pipeline.json> <collectionName>
      - run the specified pipeline on the given collection.
    """)
    exit(1)
  command = sys.argv[1]
  client = MongoClient()
  coll1 = client.test.coll1
  coll2 = client.test.coll2
  coll3 = client.test.coll3
  if command == 'fill':
    print("filling db...")
    fillDB(coll1, coll2, coll3)
  elif command == 'run':
    if len(sys.argv) < 4:
      print("you need a pipeline and a collection to run!")
      exit(1)
    filename = sys.argv[2]
    collection = sys.argv[3]
    runAggregation(filename, collection, client)
