# Geocoder

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
