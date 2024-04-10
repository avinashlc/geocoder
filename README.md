# Geocoder

- [Google geocoding API](https://developers.google.com/maps/documentation/geocoding/usage-and-billing#:~:text=for%20volume%20pricing-,Other%20usage%20limits,side%20and%20server-side%20queries.)
  - While there is no maximum number of requests per day, the following usage limit is still in place for the Geocoding API:
    - 3,000 QPM (queries per minute), calculated as the sum of client-side and server-side queries.

- Create db **[Ignore this step]**:

  ```sh
    docker run --name geocoder -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=geocoder -p 5432:5432 -d postgres
  ```

- Create `secrets.edn` file:

  ```sh
    touch resources/dev/secrets.edn
  ```

- Copy `.vscode/settings.json` file:

  ```sh
    cp .vscode/settings_example.json .vscode/settings.json
  ```

- Start the clojure repl using **calva**:

  ```sh
    ctrl+alt+c ctrl+alt+j
  ```
