package com.be.rebook.common.config;

import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.be.rebook.common.argumentresolver.auth.AuthArgumentResolver;
import com.be.rebook.common.restclients.RestClientFactory;

@Configuration
public class WebMvcConfig implements WebMvcConfigurer {

    private final DiscoveryClient discoveryClient;

    @Value("${cors.allow.origins}")
    private String[] allowedOrigins;

    public WebMvcConfig(DiscoveryClient discoveryClient) {
        this.discoveryClient = discoveryClient;
    }

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        RestClientFactory restClientFactory = new RestClientFactory(discoveryClient);
        resolvers.add(new AuthArgumentResolver(restClientFactory));
    }

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOrigins(allowedOrigins)
                .allowedMethods("GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH")
                .allowedHeaders("Content-Type", "Authorization", "access");
    }
}