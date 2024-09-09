package com.be.rebook.product.repository;

import com.be.rebook.product.dto.ProductRequestDTO;
import com.be.rebook.product.entity.Product;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.JPQLQuery;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import java.util.List;

import static com.be.rebook.product.dto.ProductRequestDTO.*;
import static com.be.rebook.product.entity.QProduct.product;


@RequiredArgsConstructor
public class ProductRepositoryImpl implements ProductRepositoryCustom {

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Page<Product> findProductsByFilter(ProductFilterDTO productFilterDTO, Pageable pageable) {

        // 서브쿼리를 통해 전체 카운트 값을 포함한 단일 쿼리 작성
        JPQLQuery<Product> query = jpaQueryFactory.selectFrom(product)
                .where(
                        universityContains(productFilterDTO.getUniversity()),
                        majorContains(productFilterDTO.getMajor()),
                        titleContains(productFilterDTO.getTitle()),
                        priceBetween(productFilterDTO.getMinPrice(), productFilterDTO.getMaxPrice())
                )
                .orderBy(getOrderSpecifier(productFilterDTO.getSortOrder()))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize());

        // 전체 개수를 가져오기 위한 서브쿼리
        long total = query.fetchCount();

        // 실제 데이터를 가져옴
        List<Product> products = query.fetch();

        return new PageImpl<>(products, pageable, total);
    }

    private BooleanExpression universityContains(String university) {
        return university != null && !university.isEmpty() ? product.university.contains(university) : null;
    }

    private BooleanExpression majorContains(String major) {
        return major != null && !major.isEmpty() ? product.major.contains(major) : null;
    }

    private BooleanExpression titleContains(String title) {
        return title != null && !title.isEmpty() ? product.title.contains(title) : null;
    }

    private BooleanExpression priceBetween(Integer minPrice, Integer maxPrice) {
        if (minPrice != null && maxPrice != null) {
            return product.price.between(minPrice, maxPrice);
        } else if (minPrice != null) {
            return product.price.goe(minPrice);
        } else if (maxPrice != null) {
            return product.price.loe(maxPrice);
        } else {
            return null;
        }
    }

    private OrderSpecifier<?> getOrderSpecifier(String order) {
        if ("asc".equalsIgnoreCase(order)) {
            return product.price.asc();
        } else if ("desc".equalsIgnoreCase(order)) {
            return product.price.desc();
        } else if ("recent".equalsIgnoreCase(order)) {
            return product.createdAt.desc();
        } else  {
            return product.createdAt.desc(); // 기본값: 최신순 정렬
        }
    }
}
