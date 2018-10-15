DOCKER = $(shell which docker)
ifeq ($(DOCKER),)
$(error "Docker not available on this system")
endif

DOCKER_COMPOSE = $(shell which docker-compose)
ifeq ($(DOCKER_COMPOSE),)
$(error "DockerCompose not available on this system")
endif

# use to override vars for your platform
ifeq (env.mk,$(wildcard env.mk))
	include env.mk
endif

all: build_imgs up tests rel

build_imgs:
	@echo "Update docker images..."
	@${DOCKER_COMPOSE} build

up:
	@${DOCKER_COMPOSE} up -d

down:
	@${DOCKER_COMPOSE} down

tests:
	@echo "Testing..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f sandbox.mk tests"

rel:
	@echo "Build release..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f sandbox.mk prod"

lint:
	@echo "Lint..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f sandbox.mk lint"

xref:
	@echo "Xref analysis..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f sandbox.mk xref"

dialyzer:
	@echo "Dialyzer..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f sandbox.mk dialyzer"

upgrade_rebar:
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f sandbox.mk upgrade_rebar"