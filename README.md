# gacsync
sync &amp; display ddl of assignments from gradescope, autograder and canvas

## Usage 

### Build

```bash
dune build
```

### Config

Copy the `user.txt.example` to `user.txt` and fill in your credentials. 
- email and password for gradescope

### Run
```bash
dune exec gacsync
```

## Roadmap

- [ ] fetch data
  - [x] gradescope
    - [x] login
    - [x] get assignments
  - [ ] autograder
    - [ ] login
    - [ ] get assignments
  - [ ] canvas
    - [ ] login
    - [ ] get assignments
    - [ ] get announcements and other info
- [ ] display
  - [ ] a simple to-do list
  - [ ] display assignments
  - [ ] display other info