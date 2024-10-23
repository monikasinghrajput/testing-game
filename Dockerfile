# Specify the OpenJDK version
FROM openjdk:11.0.13

# Set environment variables
ENV SBT_VERSION=1.4.9
ENV SCALA_VERSION=2.13.6

# Set the working directory
WORKDIR /app

# Install SBT
RUN apt-get update && \
    apt-get install -y curl && \
    curl -L -o sbt-$SBT_VERSION.deb https://repo.scala-sbt.org/scalasbt/debian/sbt-$SBT_VERSION.deb && \
    dpkg -i sbt-$SBT_VERSION.deb && \
    rm sbt-$SBT_VERSION.deb

# Optional: Verify the installation
RUN sbt sbtVersion

# Copy your project files (if applicable)
COPY . .

RUN sbt clean compile dist

# Expose port (if needed)
EXPOSE 9000

# Command to run sbt
CMD ["sbt", "run"]