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

    private final String[] ALLOWED_ORIGINS;

    public WebMvcConfig(DiscoveryClient discoveryClient, @Value("${cors.allow.origins}") String[] allowedOrigins) {
        this.discoveryClient = discoveryClient;
        this.ALLOWED_ORIGINS = allowedOrigins;
    }

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**") // 모든 엔드포인트에 대해 CORS를 허용
                .allowedOrigins(ALLOWED_ORIGINS) // 허용할 도메인
                .allowedMethods("GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS") // 허용할 HTTP 메서드
                .allowedHeaders("*") // 허용할 헤더
                .allowCredentials(true) // 자격 증명(쿠키, 인증 헤더 등)을 포함한 요청 허용
                .maxAge(3600); // 캐시의 유효 기간(초 단위)
    }

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        RestClientFactory restClientFactory = new RestClientFactory(discoveryClient);
        resolvers.add(new AuthArgumentResolver(restClientFactory));
    }
}
