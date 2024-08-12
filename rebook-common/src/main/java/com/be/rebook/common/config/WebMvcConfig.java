package com.be.rebook.common.config;

import java.util.List;

import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.be.rebook.common.argumentresolver.auth.AuthArgumentResolver;

@Configuration
public class WebMvcConfig implements WebMvcConfigurer {

    private final DiscoveryClient discoveryClient;

    public WebMvcConfig(DiscoveryClient discoveryClient) {
        this.discoveryClient = discoveryClient;
    }

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        resolvers.add(new AuthArgumentResolver(discoveryClient));
    }
}