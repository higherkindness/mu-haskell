# Docker image for a Mu Haskell service

You can use the example `Dockerfile` as a starting point. There are a few elements you need to replace:

- The `lts` version (line 1) should point to the same LTS release you are using in your `stack.yaml` file.
- The folder in the final image is here set to `mu-docker-example` (lines 7 and 9). You may want to change this.
- This `Dockerfile` exposes port 8080, since that is the one used by the example application.
- The command in line 17 is made out of the folder from the current point and the name of the executable.