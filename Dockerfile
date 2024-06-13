FROM clojure

WORKDIR /usr/src/app
ARG IM_VERSION=7.1.1-13

ENV SECRETS_PATH="/usr/src/app/resources/dev/secrets.edn"
ENV CLIP_SYSTEM="system.edn"
ENV GOOGLE_APPLICATION_CREDENTIALS="/usr/src/app/resources/gcp.json"
ENV AWS_REGION="ap-south-1"

COPY . .

RUN clojure -T:build uber

CMD java -jar target/geocoder-0.0.1-standalone.jar -w
