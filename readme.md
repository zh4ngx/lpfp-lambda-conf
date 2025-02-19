# LPFP Lambda Conf

This repository contains the code and resources for the LPFP Lambda Conf project. The project focuses on demonstrating the use of lambda functions in various programming scenarios.

## How to Run the Repository

To run the repository, follow these steps:

```sh
export SOURCE_DATE_EPOCH=$(date +%s)
```

1. **Clone the repository**:

    ```sh
    git clone https://github.com/your-username/lpfp-lambda-conf.git
    cd lpfp-lambda-conf
    ```

2. **Set the `SOURCE_DATE_EPOCH` environment variable**:

    ```sh
    export SOURCE_DATE_EPOCH=$(date +%s)
    ```

3. **Install dependencies**:
    Ensure you have the necessary dependencies installed. If the project uses a package manager like npm or pip, run the appropriate command:

    ```sh
    cabal update
    cabal install
    ```

4. **Run the project**:

    ```sh
    cabal repl
    import Mechanics3d
    ```

5. **Build the project**:

    ```sh
    cabal build
    ```

6. **Testing**:
    To run tests, use the following command:

    ```sh
    cabal test
    ```

## Repository Structure

- `src/`: Contains the source code for the project.
- `tests/`: Contains the test cases for the project.
- `docs/`: Contains documentation and resources related to the project.

For more detailed information, refer to the documentation in the `docs/` directory.