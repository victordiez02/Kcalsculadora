FROM node:20-alpine AS build

WORKDIR /app

RUN corepack enable

COPY package.json pnpm-lock.yaml .npmrc ./
RUN pnpm install --frozen-lockfile

COPY . .

ARG VITE_API_URL
ENV VITE_API_URL=$VITE_API_URL

RUN pnpm build

FROM nginx:1.27-alpine

COPY --from=build /app/dist /usr/share/nginx/html
COPY nginx.conf.template /etc/nginx/templates/default.conf.template

ENV PORT=8080
EXPOSE 8080

CMD ["nginx", "-g", "daemon off;"]
