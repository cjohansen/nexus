VERSION := $(shell sed 's/xmlns="[^"]*"//g' pom.xml | xmllint --xpath 'string(/project/version)' -)

clean:
	rm -fr target dev-resources/public/app dev-resources/public/portfolio

nexus.jar: $(wildcard src/nexus/*.clj*)
	clojure -M:jar

deploy: nexus.jar
	env CLOJARS_USERNAME=$$(xmllint --xpath "string(/settings/servers/server[id='clojars']/username)" ~/.m2/settings.xml) \
	    CLOJARS_PASSWORD=$$(xmllint --xpath "string(/settings/servers/server[id='clojars']/password)" ~/.m2/settings.xml) \
	    clojure -X:deploy

.PHONY: clean deploy
