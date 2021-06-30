from bson.objectid import ObjectId

def mongoIdMaker(x):
  
  id = ObjectId(bytes(x, encoding='utf-8'))
  id = str(id)
  return id
