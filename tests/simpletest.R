app <- ShinyDriver$new("../", loadTimeout = 10000)
app$snapshotInit("simpletest")

app$setInputs(session = "Full Plot")
app$snapshot()
