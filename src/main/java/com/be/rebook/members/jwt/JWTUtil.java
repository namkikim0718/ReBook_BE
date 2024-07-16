package com.be.rebook.members.jwt;

import io.jsonwebtoken.Jwts;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.Date;

@Component
public class JWTUtil {
    private final SecretKey secretKey;
    private JWTUtil(@Value("${spring.jwt.secret}") String secret){
        this.secretKey = new SecretKeySpec(
                secret.getBytes(StandardCharsets.UTF_8),
                Jwts.SIG.HS256.key().build().getAlgorithm());
    }

    public String getUsername(String token){
        return Jwts.parser()
                .verifyWith(secretKey)
                .build().parseSignedClaims(token)
                .getPayload()
                .get("username",String.class);
    }
    public String getRole(String token){
        return Jwts.parser()
                .verifyWith(secretKey)
                .build().parseSignedClaims(token)
                .getPayload()
                .get("Role",String.class);
    }
    public Boolean isExpired(String token){

        System.out.println("JWTUtil isExpired 동작");
        System.out.println("username "+getUsername(token));
        System.out.println("role "+getRole(token));

        Date now  = new Date();
        long accessTokenLength = 10*60*1000;
        Date newTime = new Date(now.getTime()+accessTokenLength);

        return Jwts.parser()
                .verifyWith(secretKey)
                .build().parseSignedClaims(token)
                .getPayload()
                .getExpiration().before(newTime);
    }

    //토큰 종류 구별용
    public String getCategory(String token){
        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload().get("category", String.class);
    }

    public String createJwt(String category, String username, String role, Long expiredMs) {

        return Jwts.builder()
                .claim("category", category)
                .claim("username", username)
                .claim("role", role)
                .issuedAt(new Date(System.currentTimeMillis()))
                .expiration(new Date(System.currentTimeMillis() + expiredMs))
                .signWith(secretKey)
                .compact();
    }
}
