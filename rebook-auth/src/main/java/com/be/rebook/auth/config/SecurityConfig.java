package com.be.rebook.auth.config;

import com.be.rebook.auth.repository.RefreshTokensRepository;
import com.be.rebook.auth.utility.CookieUtil;
import com.be.rebook.auth.jwt.CustomLogoutFilter;
import com.be.rebook.auth.jwt.JWTFilter;
import com.be.rebook.auth.jwt.JWTUtil;
import com.be.rebook.auth.jwt.LoginFilter;
import com.be.rebook.auth.oauth.handler.OAuthAuthenticationSuccessHandler;
import com.be.rebook.auth.oauth.service.CustomOAuth2UserService;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.authentication.logout.LogoutFilter;
import org.springframework.web.cors.CorsConfiguration;

import java.util.Arrays;
import java.util.Collections;

@Configuration
@EnableWebSecurity
public class SecurityConfig {
    private final AuthenticationConfiguration authenticationConfiguration;
    private final JWTUtil jwtUtil;
    private final CookieUtil cookieUtil;

    private final CustomOAuth2UserService customOAuth2UserService;
    private final OAuthAuthenticationSuccessHandler oAuthAuthenticationSuccessHandler;

    private final RefreshTokensRepository refreshTokensRepository;

    public SecurityConfig(AuthenticationConfiguration authenticationConfiguration, JWTUtil jwtUtil,
            CookieUtil cookieUtil,
            RefreshTokensRepository refreshTokensRepository, CustomOAuth2UserService customOAuth2UserService,
            OAuthAuthenticationSuccessHandler oAuthAuthenticationSuccessHandler) {
        this.authenticationConfiguration = authenticationConfiguration;
        this.jwtUtil = jwtUtil;
        this.cookieUtil = cookieUtil;
        this.refreshTokensRepository = refreshTokensRepository;
        this.customOAuth2UserService = customOAuth2UserService;
        this.oAuthAuthenticationSuccessHandler = oAuthAuthenticationSuccessHandler;
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration configuration)
            throws Exception {
        return configuration.getAuthenticationManager();
    }

    @Bean
    public BCryptPasswordEncoder bCryptPasswordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {

        http.cors(cors -> cors
                .configurationSource(request -> {
                    CorsConfiguration configuration = new CorsConfiguration();
                    configuration.setAllowedOrigins(Collections.singletonList("https://www.rebook45.link"));
                    configuration.setAllowedMethods(Collections.singletonList("*"));
                    configuration.setAllowCredentials(true);
                    configuration.setAllowedHeaders(Collections.singletonList("*"));
                    configuration.setMaxAge(3600L);
                    configuration.setExposedHeaders(Arrays.asList("Authorization", "access"));

                    return configuration;
                }));

        http.csrf(auth -> auth.disable());
        http.formLogin(auth -> auth.disable());
        http.httpBasic(auth -> auth.disable());

        http.authorizeHttpRequests(auth -> auth
                .requestMatchers("/auth/signin", "/", "/auth/members/signup", "/auth/members/signup/*").permitAll()
                .requestMatchers("/auth/oauth2/code/*").permitAll()

                //fixme : requestMatchers에 잡히지 않으면 ("/auth/members/refreshtoken" 로 되있었을때) OAuth .redirectionEndPoint로 리다이렉션 때려버림.. 대체 왜죠????
                .requestMatchers("/auth/members/refreshtoken/reissue", "/auth/members/password/reset").permitAll()
                .requestMatchers("/error").permitAll()
                .requestMatchers("/").permitAll() //테스트용 루트경로 혀용
                .anyRequest().authenticated());

        http.addFilterAfter(new JWTFilter(jwtUtil), LoginFilter.class);
        http.addFilterAt(
                new LoginFilter(authenticationManager(authenticationConfiguration), jwtUtil, refreshTokensRepository,
                        cookieUtil),
                UsernamePasswordAuthenticationFilter.class);
        http.addFilterBefore(new CustomLogoutFilter(jwtUtil, refreshTokensRepository), LogoutFilter.class);

        http.oauth2Login((oauth2Login) -> oauth2Login
                .authorizationEndpoint(endpoint -> endpoint.baseUri("/auth/oauth2/signin"))
                .redirectionEndpoint(endpoint -> endpoint.baseUri("/auth/oauth2/code/*"))
                .userInfoEndpoint((userInfoEndpoint) -> userInfoEndpoint
                        .userService(customOAuth2UserService))
                .successHandler(oAuthAuthenticationSuccessHandler));

        http.sessionManagement(session -> session
                .sessionCreationPolicy(SessionCreationPolicy.STATELESS));

        return http.build();
    }
}
